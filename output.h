#include <memory.h>
#include <assert.h>


#ifndef ACTOR_MAX_NAME_LENGTH
#define ACTOR_MAX_NAME_LENGTH (64)
#endif
#ifndef ACTOR_MAX_NAME_LENGTH
#define ACTOR_MAX_NAME_LENGTH (64)
#endif
#ifndef MAX_ITEMS_ON_LOCATION
#define MAX_ITEMS_ON_LOCATION (32)
#endif
enum InterfaceElements {
  MSG_TELLACTORNAME = 8, // tellActorName
  MSG_TELLITEMSONLOCATION = 16, // tellItemsOnLocation

  INTERFACE_ELEMENT_COUNT
};

//declare tellActorName @ MSG_TELLACTORNAME = 8
//  actor_id  : uint(16)
//  name      : string (ACTOR_MAX_NAME_LENGTH 64)
//  last_name : string (ACTOR_MAX_NAME_LENGTH 64)
template <typename CallbackFunction>
bool tellActorName(
    unsigned int actor_id,
    const char *name,
    size_t name_length,
    const char *last_name,
    size_t last_name_length,
    CallbackFunction callback // [&](const char *buffer, size_t bytes) -> bool { ... }
  ) {
  char __buffer[sizeof(unsigned short)+(ACTOR_MAX_NAME_LENGTH * sizeof(char))+(ACTOR_MAX_NAME_LENGTH * sizeof(char))+4];
  const char *__write_end = __buffer + sizeof(__buffer);
  #ifdef _DEBUG
  *(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) = 0x7C7C7C7C;
  #endif
  char *__write_ptr = __buffer;
  *(unsigned int*)(__write_ptr) = MSG_TELLACTORNAME;
  __write_ptr += sizeof(unsigned short);
  {
  unsigned int __bits = 0;
  __bits |= (name_length) & ((~(unsigned int)0) >> (32 - 7));
  __bits <<= 7;
  __bits |= (last_name_length) & ((~(unsigned int)0) >> (32 - 7));
  __bits <<= 7;
  __bits |= (actor_id) & ((~(unsigned int)0) >> (32 - 16));
  __bits <<= 16;
  *(unsigned int*)__write_ptr = __bits;
  __write_ptr += 4;
  }
  if (name_length > ACTOR_MAX_NAME_LENGTH) return false;
  memcpy(__write_ptr, name, sizeof(char) * name_length); // TODO: compress strings
  __write_ptr += sizeof(char) * name_length;
  if (last_name_length > ACTOR_MAX_NAME_LENGTH) return false;
  memcpy(__write_ptr, last_name, sizeof(char) * last_name_length); // TODO: compress strings
  __write_ptr += sizeof(char) * last_name_length;
  assert(*(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) == 0x7C7C7C7C);
  return callback(__buffer, __write_ptr - __buffer);
}

//declare tellItemsOnLocation @ MSG_TELLITEMSONLOCATION = 16
//  x : float(fixed16)
//  y : float(fixed16)
//  items : array[MAX_ITEMS_ON_LOCATION 32] uint(10)
template <typename CallbackFunction>
bool tellItemsOnLocation(
    float x,
    float y,
    const unsigned int *items,
    size_t items_count,
    CallbackFunction callback // [&](const char *buffer, size_t bytes) -> bool { ... }
  ) {
  char __buffer[sizeof(unsigned short)+4];
  const char *__write_end = __buffer + sizeof(__buffer);
  #ifdef _DEBUG
  *(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) = 0x7C7C7C7C;
  #endif
  char *__write_ptr = __buffer;
  *(unsigned int*)(__write_ptr) = MSG_TELLITEMSONLOCATION;
  __write_ptr += sizeof(unsigned short);
  {
  unsigned int __bits = 0;
  __bits |= (items_count) & ((~(unsigned int)0) >> (32 - 6));
  __bits <<= 6;
  *(unsigned int*)__write_ptr = __bits;
  __write_ptr += 1;
  }
  if (items_count > MAX_ITEMS_ON_LOCATION) return false;
  assert(*(unsigned int*)(__buffer + sizeof(__buffer) - sizeof(unsigned int)) == 0x7C7C7C7C);
  return callback(__buffer, __write_ptr - __buffer);
}

template <typename Class>
struct EditorFunctionTable {
  typedef bool (Class::*tellActorName_t)(unsigned int actor_id, const char *name, size_t name_length, const char *last_name, size_t last_name_length);
  tellActorName_t tellActorName;

  typedef bool (Class::*tellItemsOnLocation_t)(float x, float y, const unsigned int *items, size_t items_count);
  tellItemsOnLocation_t tellItemsOnLocation;

};

template <typename Class>
bool decode(const char *buffer, size_t bytes, Class *object, EditorFunctionTable<Class> *functions) {
  const char *__read_ptr = buffer + sizeof(unsigned short);
  const char *__read_end = buffer + bytes;
  if (!buffer || __read_ptr > __read_end) return false;
  unsigned int __msg = *(unsigned short*)buffer;
  switch (__msg) {
  case MSG_TELLACTORNAME: {
    unsigned int name_length;
    unsigned int last_name_length;
    unsigned int actor_id;
    char name[ACTOR_MAX_NAME_LENGTH+1];
    char last_name[ACTOR_MAX_NAME_LENGTH+1];
  
    if (bytes < (sizeof(unsigned short))) return false;
    if (functions && !functions->tellActorName) return false;
    {
    unsigned int __bits = *(unsigned int*)__read_ptr;
    actor_id = __bits & ((~(unsigned int)0) >> (32-16));
    __bits >>= 16;
    last_name_length = __bits & ((~(unsigned int)0) >> (32-7));
    __bits >>= 7;
    name_length = __bits & ((~(unsigned int)0) >> (32-7));
    __bits >>= 7;
    __read_ptr += 4;
    }
    if (name_length > ACTOR_MAX_NAME_LENGTH) return false;
    if (__read_ptr + (sizeof(char) * name_length) > __read_end) return false;
    memcpy(name, __read_ptr, sizeof(char) * name_length);
    __read_ptr += sizeof(char) * name_length;
    name[name_length] = '\0';
    if (last_name_length > ACTOR_MAX_NAME_LENGTH) return false;
    if (__read_ptr + (sizeof(char) * last_name_length) > __read_end) return false;
    memcpy(last_name, __read_ptr, sizeof(char) * last_name_length);
    __read_ptr += sizeof(char) * last_name_length;
    last_name[last_name_length] = '\0';
    return !functions || (object->*(functions->tellActorName))(actor_id, name, name_length, last_name, last_name_length);
  }
  case MSG_TELLITEMSONLOCATION: {
    unsigned int items_count;
    float x;
    float y;
  
    if (bytes < (sizeof(unsigned short))) return false;
    if (functions && !functions->tellItemsOnLocation) return false;
    {
    unsigned int __bits = *(unsigned int*)__read_ptr;
    items_count = __bits & ((~(unsigned int)0) >> (32-6));
    __bits >>= 6;
    __read_ptr += 1;
    }
    if (items_count > MAX_ITEMS_ON_LOCATION) return false;
    return !functions || (object->*(functions->tellItemsOnLocation))(x, y, items, items_count);
  }
  }
  return false; // todo: log error
}
