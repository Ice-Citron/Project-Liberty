#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#define CONTAINER_OF(ptr, type, member) \
    ((type *) ((char *) (ptr) - offsetof(type, member)))

struct link {
    struct link *prev;
    struct link *next;
};

struct order {
    int price_pence;
    int quantity;
    struct link link;
};

typedef bool link_less_fn(const struct link *a, const struct link *b);

void link_list_init(struct link *head) {
    /* makes an empty circular list, where `head` is a sentinel element*/
    head->prev = head;
    head->next = head;
}

void insert_sorted(struct link *head, struct link *elem, link_less_fn *less) {
    struct link *cursor = head->next;
    while (cursor != head && !less(elem, cursor)) {
        cursor = cursor->next;
    }
    elem->next = cursor;
    elem->prev = cursor->prev;
    cursor->prev->next = elem;
    cursor->prev = elem;
}// HEAD(start here) 1 2 4 10(elem) 11(cursor) 20 HEAD

bool order_price_less(const struct link *a, const struct link *b) {
    struct order *reconstructed_a = CONTAINER_OF(a, struct order, link);
    struct order *reconstructed_b = CONTAINER_OF(b, struct order, link);
    return reconstructed_a->price_pence < reconstructed_b->price_pence;
}

int main(int argc, char **argv) {
    struct order first_order = {
        .price_pence = 50,
        .quantity = 3,
    };
    struct link head;
    link_list_init(&head);
    insert_sorted(&head, &first_order.link, &order_price_less);

    for (int i = 0; i < 11; i++) {
        struct order *temp_order = malloc(sizeof(struct order));
        if (temp_order == NULL) {
            perror("Failed to allocate memory for `struct order`\n");
            return 1;
        }
        temp_order->price_pence = 330 - i * 30;
        temp_order->quantity = 67;
        insert_sorted(&head, &temp_order->link, &order_price_less);
    }

    printf("Printing intrinsic linked list contents:\n");

    int i = 0;
    for (struct link *elem = head.next; elem != &head; elem = elem->next) {
        struct order *reconstructed_order = CONTAINER_OF(elem, struct order, 
                                                         link);
        printf("Item %d's price_pence is: %d\n", i, 
               reconstructed_order->price_pence);
        printf("Item %d's quantity is: %d\n", i, 
               reconstructed_order->quantity);
        i++;
    }
    return 0;
}
