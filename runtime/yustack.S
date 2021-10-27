
// The beginning of the current stack segment (growing downward).
// The first (highest) valid stack segment address is CURR_SEG_BEGIN - 8.
#define CURR_SEG_BEGIN %fs:0x80

// The end of the current stack segment (growing downward).
// This address is the last (lowest) invalid address on the sack segment.
#define CURR_SEG_END %fs:0x70

#define SYSTEM_STACK %fs:0x88


.section .data

.globl yur_stack_overlap_size
.type yur_stack_overlap_size, @object
yur_stack_overlap_size: .quad 672

.globl yur_stack_segment_size
.type yur_stack_segment_size, @object
yur_stack_segment_size: .quad 8096

.globl yur_reserved_stack_size
.type yur_reserved_stack_size, @object
yur_reserved_stack_size: .quad 400


.text

.globl __yu_morestack
.type __yu_morestack, @function
__yu_morestack:
.cfi_startproc
  // Save old stack pointer before switching stack:
  movq %rsp, %r11

  // Switch to system stack, which we assume is properly aligned.
  movq SYSTEM_STACK, %rsp

  // Save registers which may be used as functions arguments
  // to our caller. We need to restore these before returning.
  subq $80, %rsp
  movq %rdi, (%rsp)
  movq %rsi, 8(%rsp)
  movq %rdx, 16(%rsp)
  movq %rcx, 24(%rsp)
  movq %r8, 32(%rsp)
  movq %r9, 40(%rsp)
  // Preserve rax as well, since it is used for variadic arguments.
  movq %rax, 48(%rsp)

  // Save old stack pointer in rcx, argument to
  // call yur_new_stack_seg_s later on:
  movq %r11, %rcx
  // Move the caller's frame size to argument register r8,
  // which is used for calling yur_new_stack_seg_s later on:
  movq %r10, %r8

  // At this point we have the following registers we can clobber:
  // rax,r10,r11,rdi,rsi,rdx,r9

  // Now, compute the amount of stack space to copy from old stack.
  // It will copy at least one stack frame, even if that frame
  // is larger than yur_stack_overlap_size.

  // Save yur_stack_overlap_size in regsiter rax for efficient access:
  movq yur_stack_overlap_size, %rax
  // Save CURR_SEG_BEGIN and CURR_SEG_END for fast access and as
  // arguments to yur_new_stack_seg_s later on:
  movq CURR_SEG_BEGIN, %rsi
  movq CURR_SEG_END, %rdx

  // The following registers are free:
  // r10,r11,rdi,r9

  // The current frame pointer register rbp is the frame of our
  // caller's caller, since our caller has not entered the "normal"
  // prologue yet. Compute the "minimal" amount of stack space to
  // copy, which is the previous stack frame (pointed to by the frame
  // pointer) including an additional return address, which was pushed
  // when calling this function.
  movq %rbp, %rdi
  subq %rcx, %rdi
  addq $8, %rdi
  // Save caller's caller stack frame size in r9, which is argument
  // to call yur_new_stack_seg_s later on.
  movq %rdi, %r9
  // Save it on the stack as well, since we need it after calling
  // yur_new_stack_seg_s as well.
  movq %rdi, 56(%rsp)

  // The following registers are free:
  // r10,r11

  // If this is above or equal to yur_stack_overlap_size, then we have
  // the amount of stack space to copy over already.
  cmpq %rax, %rdi
  jae .copy_size_found

  // Here we need to backtrack the stack to compute the amount
  // of stack to copy over. We can clobber rbp, since we will
  // set it to a new value before returning.

  // The below loop clobbers register r11, and r10 is still free.

.add_stack_frame:
  // Get the next frame pointer in the sequence:
  movq (%rbp), %r11
  // If the next frame pointer is the beginning of the current stack
  // segment then we cannot copy any more. Hence have found the
  // amount to copy:
  cmpq %rsi, %r11
  je .copy_size_found
  // Get the size of the next stack frame (pointed to by r11):
  subq %rbp, %r11
  // Add the size of this stack frame to the total:
  addq %r11, %rdi
  // Compare the total to yur_stack_overlap_size:
  cmpq %rax, %rdi
  // For potential next iteration, save next frame pointer:
  movq (%rbp), %rbp
  // If still below yur_stack_overlap_size then add one more frame:
  jb .add_stack_frame
  // Here the total is above or equal to yur_stack_overlap_size.
  // If we are spot on equal, then we have found the amount to copy:
  je .copy_size_found
  // Otherwise we have one stack frame too much, subtrack it:
  subq %r11, %rdi

.copy_size_found:
  // Preserve the size to copy from old stack, since we need it after
  // call yur_new_stack_seg_s:
  movq %rdi, 64(%rsp)

  // Here:
  // - rdi (first argument) is the size to copy from old segment.
  // - rsi (second argument) is pointing to the old segment beginning.
  // - rdx (third argument) is pointing to the old segment end.
  // - rcx (fourth argument) is the old stack pointer.
  // - r8 (fifth argument) is the caller frame size.
  // - r9 (sixth argument) is the caller's caller frame size.
  call yur_new_stack_seg_s

  // We have a new initialized stack segment with beginning pointed
  // to by register rax and end pointed to by register rdx.
  // Update to use the beginning and end of the new stack:
  movq %rax, CURR_SEG_BEGIN
  movq %rdx, CURR_SEG_END
  // Preserve the beginning of the stack segment in r10:
  movq %rax, %r10

  // Restore the caller's registers:
  movq (%rsp), %rdi
  movq 8(%rsp), %rsi
  movq 16(%rsp), %rdx
  movq 24(%rsp), %rcx
  movq 32(%rsp), %r8
  movq 40(%rsp), %r9
  movq 48(%rsp), %rax

  // Get size of caller's caller stack frame before switching stack:
  movq 56(%rsp), %r11
  // Subtract size copied from old stack:
  subq 64(%rsp), %r10
  // Switch to the new stack:
  movq %r10, %rsp
  // Add size of caller's caller stack frame and subtract 8
  // to get the new frame pointer:
  lea -8(%r11,%r10), %rbp
  // The return address is copied from the old stack to the top of
  // the new stack. Return to caller:
  ret
.cfi_endproc


.globl __yu_lessstack
.type __yu_lessstack, @function
__yu_lessstack:
.cfi_startproc
  // On entry here we have rsp == CURR_SEG_BEGIN.
  // Save rsp in register r10 before switching stack:
  movq %rsp, %r10

  // Switch to system stack, which we assume is properly aligned:
  movq SYSTEM_STACK, %rsp

  // We need to preserve registers rax and rdx, since these registers
  // may contain return value from caller.
  subq $32, %rsp
  movq %rax, (%rsp)
  movq %rdx, 8(%rsp)

  // Save the top stack segment ending in register rdi, used as
  // argument to call yur_delete_stack_seg_s later on.
  movq CURR_SEG_END, %rdi

  // Get the previous stack pointer, r10->prev_sp:
  movq (%r10), %rax
  // Get the previous segment beginning, r10->prev_seg_begin:
  movq 8(%r10), %rdx
  // Get the previous segment end, r10->prev_seg_end:
  movq 16(%r10), %r11
  // Get the previous frame pointer, r10->prev_bp:
  movq 24(%r10), %r10

  // Update global state to the previous segment pointers:
  movq %rdx, CURR_SEG_BEGIN
  movq %r11, CURR_SEG_END

  // Preserve the previous frame pointer and stack pointer:
  movq %r10, 16(%rsp)
  movq %rax, 24(%rsp)

  // Delete the top stack segment with end pointer in rdi:
  call yur_delete_stack_seg_s

  // Restore potential return values from caller:
  movq (%rsp), %rax
  movq 8(%rsp), %rdx

  // Switch to the previous frame pointer and stack pointer:
  movq 16(%rsp), %rbp
  movq 24(%rsp), %rsp

  // This (the previous) stack spointer can potantially be at it's
  // beginning as well. Check this before returning:
  cmpq CURR_SEG_BEGIN, %rsp
  // If the stack pointer is at the beginning, then repeat:
  je __yu_lessstack

  // If everythng turns out right, then the return address on top
  // of the stack will return back to our caller's caller:
  ret
.cfi_endproc


.globl yur_run
.type yur_run, @function
yur_run:
.cfi_startproc
  // Add stack frame:
  subq $24, %rsp
  // Preserve continuation, its argument, and the current system stack:
  movq %rdi, (%rsp)
  movq %rsi, 8(%rsp)
  movq SYSTEM_STACK, %rax
  movq %rax, 16(%rsp)

  // Set the new system stack:
  movq %rsp, SYSTEM_STACK

  // Get a new stack segment:
  movq $24, %rdi
  call yur_initial_stack_seg_s
  // Set the new stack segment boundaries:
  movq %rax, CURR_SEG_BEGIN
  movq %rdx, CURR_SEG_END

  // Get the continuation:
  movq (%rsp), %rsi
  // Get the continuation's argument:
  movq 8(%rsp), %rdi
  // Preserve system frame pointer:
  movq %rbp, (%rsp)

  // Switch to the new stack:
  movq %rax, %rsp
  // Aligh stack:
  pushq $0
  // Push initial "dummy" frame pointer:
  pushq %rax
  // Setup the new frame pointer:
  movq %rsp, %rbp
  // Aligh stack:
  pushq $0

  // Call the continuation:
  call *%rsi

  // Switch back to system stack:
  movq SYSTEM_STACK, %rsp

  // Restore previous system stack:
  movq 16(%rsp), %rdi
  movq %rdi, SYSTEM_STACK

  // Preserve result of continuation:
  movq %rdx, 8(%rsp)
  movq %rax, 16(%rsp)

  // Delete the remaining stack segment(s):
  movq CURR_SEG_BEGIN, %rdi
  movq CURR_SEG_END, %rsi
  call yur_delete_all_stack_segs_s

  // Restore system frame pointer:
  movq (%rsp), %rbp
  // Restore result of continuation:
  movq 8(%rsp), %rdx
  movq 16(%rsp), %rax
  // Remove stack frame and return:
  addq $24, %rsp
  ret
.cfi_endproc
