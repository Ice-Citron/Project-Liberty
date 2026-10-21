1. git clone git@gitlab.doc.ic.ac.uk:lab2627_autumn/pintos_task0_shn125.git

2. The problem is that `strcpy()` never checks the destination size, which means
   a source string that's larger than the destination buffer overflows it. 
   This is a buffer overflow error which corrupts data silently or crashes the
   program. Additionally, `strcpy()` only stops copying from source 
   string to destination buffer when it hits a `'\0'`. This means that if `'\0'` 
   was accidentally omitted, `strcpy()` will keep going until it finds a zero 
   byte anywhere in the memory.

3. Result logs: src/devices/build/tests/devices/alarm-multiple.result
   Output path: src/devices/build/tests/devices/alarm-multiple.output

4. a. In PintOS, each thread only gets a 4 kB memory page. Given the thread's 
      execution stack grows downward from the top of the page toward 
      `struct thread`, the maximum size of the execution stack is 4 kB minus 
      `sizeof(struct thread)`. Hence, `struct thread` should be well under 1 
      kB such that the execution stack has enough room for storage.

      Additionally, given the execution stack's size is limited, large data 
      structures like non-static local arrays or structs shouldn't be
      allocated on the execution stack, and should instead be stored on the heap
      with `malloc ()`.

   b. Stack overflow occurs when the size of data stored in the execution
      stack is larger than the maximum size of the execution stack, resulting
      in `struct thread` in the thread being overwritten. PintOS identifies a 
      stack overflow occurring by asserting `is_thread()` which checks that
      `struct thread`'s magic number member (`unsigned magic`) always equals 
      `THREAD_MAGIC` macro constant (failure causes kernel panic). This works 
      because it's the last member of `struct thread` which is at the top of the struct, meaning as an overflowing stack grows downwards, it will 
      overwrite `magic` first. 

5. 

