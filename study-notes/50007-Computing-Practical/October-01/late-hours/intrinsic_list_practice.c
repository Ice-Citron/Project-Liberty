#include <stdio.h>
#include <stddef.h> // required for offsetof()

#define CONTAINER_OF(ptr, type, member) \
    ((type *) ((char *)(ptr) - offsetof(type, member)))

struct link {
    struct link *prev;
    struct link *next;
};

struct Thread {
    int thread_id;
    char *name;
    struct link ready_link;
    struct link sleep_link;
};

void wake_up_task(struct link *timer_ptr) {
    struct Thread *thread = CONTAINER_OF(timer_ptr, struct Thread, sleep_link);
    printf("--> wake_up_task recovered Thread ID: %d (Name: %s)\n", 
           thread->thread_id, thread->name);
}

int main(int argc, char **argv) {
    struct Thread new_thread = {
        .thread_id = 77,
        .name = "Sleeping Thread"
    };

    // The CPU scheduler only holds a pointer to ready_link
    struct link *ready_link_ptr = &new_thread.ready_link;
    // The Timer only holds a pointer to a sleep_link
    struct link *sleep_link_ptr = &new_thread.sleep_link;

    printf("Address in memory:\n");
    printf("Original Thread head: %p\n", (void *)&new_thread);
    printf("Original Thread head's ready link: %p\n", 
           (void *)&new_thread.ready_link);
    printf("Original Thread head's sleep link: %p\n", 
           (void *)&new_thread.sleep_link);
    
    
    printf("Thread recovered from ptr: %p\n", 
           CONTAINER_OF(ready_link_ptr, struct Thread, ready_link));
    printf("Thread's recovered from ptr: %p\n", 
           CONTAINER_OF(sleep_link_ptr, struct Thread, sleep_link));
}

// . -> (left to right) & (right to left)
