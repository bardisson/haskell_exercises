// hash_table.c
#include <stdlib.h>
#include <string.h>

#include "hash_table.h"

// initialization function for ht_item(s)
// marked static becuase this will only every be called by code internal to the hash hash_table
/**
  * @brief Initializes ht_item(). Marked static becuase this will only be called by code internal to the hash table.
  *  @param k The hash_item's key.
  *  @param v The hash_items's value.
  *  @return ht_item*
**/
static ht_item* ht_new_item(const char* k, const char* v) {
  // allocate memory
  ht_item* i = malloc(sizeof(ht_item));
  // assign keys and values
  i->key = strdup(k); // strdup calculates size of string, adds null term byte, and assigns memory space via malloc()
  i->value = strdup(v);
  return i;
}

ht_hash_table* ht_new() { 
  ht_hash_table* ht = malloc(sizeof(ht_hash_table)); //malloc returns a void ptr to the first byte of assigned memory block in the heap; implicitly converted to ht_hash_table*
  ht -> size = 53;
  ht -> count = 0;
  ht -> items = calloc((size_t)ht->size,sizeof(ht_item*));
  return ht;
}

static void ht_del_item(ht_item* i) {
  free(i->key);
  free(i->value);
  free(i);
}

/**
* @brief Deletes the ht_hash_table by removing non-NULL item entries and freeing allocated memory. 
*
**/
void ht_del_hash_table(ht_hash_table* ht) {
  for (int i = 0; i < ht->size; i++) {
    ht_item* item = ht->items[i];
    if (item != NULL) {
      ht_del_item(item);
    }
  }
  free(ht->items);
  free(ht);
}

