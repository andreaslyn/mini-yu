#include "yubase.h"

#ifndef yur_DISABLE_SPLIT_STACK

.text

.globl yur_oneshot_jmp
.type yur_oneshot_jmp, @function
.p2align 4
yur_oneshot_jmp:
.cfi_startproc
  // Add stack frame:
  subq $24, %rsp
  // Preserve wrap function, the prompt argument, and the callback:
  movq %rdi, (%rsp)
  movq %rsi, 8(%rsp)
  movq %rdx, 16(%rsp)

  // Save currrent stack pointer in r15:
  // We do not need to preserve r15, since we never return to caller:
  movq %rsp, %r15
  // Switch to system stack:
  movq SYSTEM_STACK, %rsp

  // Get a new stack segment:
  movq $8, %rdi
  call yur_initial_stack_seg_s
  // Set the new stack segment boundaries:
  movq %rax, CURR_SEG_BEGIN
  movq %rdx, CURR_SEG_END

  // Get the wrap function, the prompt argument, and the callback:
  movq (%r15), %r8
  movq 8(%r15), %rdi
  movq 16(%r15), %rsi

  // Switch to the new stack:
  movq %rax, %rsp
  // Push initial "dummy" frame pointer:
  pushq %rax
  // Setup the new frame pointer:
  movq %rsp, %rbp

  // Align stack like a call instruction and jump to wrap function:
  call *%r8
.cfi_endproc


// Function yur_setjmp is based on musl libc, MIT license.
.globl yur_setjmp
.type yur_setjmp, @function
.p2align 4
yur_setjmp:
.cfi_startproc
  movq %rbx, (%rdi)         /* rdi is jmp_buf, move registers onto it */
  movq %rbp, 8(%rdi)
  movq %r12, 16(%rdi)
  movq %r13, 24(%rdi)
  movq %r14, 32(%rdi)
  movq %r15, 40(%rdi)
  leaq 8(%rsp), %rdx        /* this is our rsp WITHOUT current ret addr */
  movq %rdx, 48(%rdi)
  movq (%rsp), %rdx         /* save return addr ptr for new rip */
  movq %rdx, 56(%rdi)
  xorl %eax, %eax           /* always return 0 */
  cmpq %rbp, %rsp
  je __yu_lessstack
  ret
.cfi_endproc


// Function yur_longjmp is based on musl libc, MIT license.
.globl yur_longjmp
.type yur_longjmp, @function
.p2align 4
yur_longjmp:
  movq (%rdi),%rbx         /* rdi is the jmp_buf, restore regs from it */
  movq 8(%rdi),%rbp
  movq 16(%rdi),%r12
  movq 24(%rdi),%r13
  movq 32(%rdi),%r14
  movq 40(%rdi),%r15
  movq 48(%rdi),%rsp
  movl %esi, %eax
  jmp *56(%rdi)           /* goto saved address without altering rsp */

#endif // yur_DISABLE_SPLIT_STACK
