
#include <stdio.h>
#include <stdlib.h>
struct int_list {
int val;
struct int_list* next;
};
typedef struct int_list int_list;
void print_hello() {
printf("%s\n", "Hello, world.");
return;
}

void print_int_list(int_list* list) {
int_list* it;

for ((it = list); it; (it = (it->next))) {
printf("%d ", (it->val));
}

printf("\n");
return;
}

int main(int argc, char** argv) {
int_list* l1 = malloc((sizeof(int_list) * 3LL));
int_list* l2 = (l1 + 1LL);
int_list* l3 = (l1 + 2LL);

((l1->val) = 3LL);
((l1->next) = l2);
((l2->val) = 5LL);
((l2->next) = l3);
((l3->val) = 9LL);
((l3->next) = NULL);
print_int_list(l1);
print_hello();
return EXIT_SUCCESS;
}

 