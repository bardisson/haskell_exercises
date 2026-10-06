// hash_table.h 
typedef struct {
   char* key;
   char* value;
 } ht_item;

typedef struct {
  int size;
  int count;
  ht_item** items; // double pointer stores the address of a pointer to the address of a variable
} ht_hash_table;

// function protoypes
ht_hash_table* ht_new(void);
void ht_del_hash_table(ht_hash_table *ht);
