#include "yubase.h"


.text

#define lessstack_ret cmpq %rbp, %rsp; je __yu_lessstack; ret


#define INC(reg, next) \
	testw $0xFE, 16(%reg); \
	jnz 1f; \
	incq (%reg); \
	next; \
1:; \
	cmpw $2, 16(%reg); \
	jne 1f; \
	next; \
1:; \
	lock incq (%reg)

.globl yur_inc_6
.globl yur_inc_5
.globl yur_inc_4
.globl yur_inc_3
.globl yur_inc_2
.globl yur_inc_1
.type yur_inc_6, @function
.type yur_inc_5, @function
.type yur_inc_4, @function
.type yur_inc_3, @function
.type yur_inc_2, @function
.type yur_inc, @function
.align 1024
yur_inc_6:
.cfi_startproc
  INC(r9, jmp yur_inc_5)
yur_inc_5:
  INC(r8, jmp yur_inc_4)
yur_inc_4:
  INC(rcx, jmp yur_inc_3)
yur_inc_3:
  INC(rdx, jmp yur_inc_2)
yur_inc_2:
  INC(rsi, jmp yur_inc_1)
yur_inc_1:
  INC(rdi, lessstack_ret)
  cmpq %rbp, %rsp
  je __yu_lessstack
  ret
.cfi_endproc


#define DEC(reg, next) \
  testw $0xFFFF, 16(%reg); \
  jnz 1f; \
  decq (%reg); \
  jz 3f; \
  next; \
1:; \
  cmpw $2, 16(%reg); \
  jne 1f; \
  next; \
1:; \
  cmpw $3, 16(%reg); \
  jne 1f; \
  lock decq (%reg); \
  jz 3f; \
  next; \
1:; \
  cmpw $1, 16(%reg); \
  jne 1f; \
  decq (%reg); \
  jz 2f; \
  next; \
1:; \
  lock decq (%reg); \
  jz 2f; \
  next; \
2:; \
  pushq %reg; \
  call destructor_dealloc; \
  popq %reg; \
  next; \
3:; \
  pushq %reg; \
  call pure_dealloc; \
  popq %reg

.globl yur_unref_6
.globl yur_unref_5
.globl yur_unref_4
.globl yur_unref_3
.globl yur_unref_2
.globl yur_unref_1
.type yur_unref_6, @function
.type yur_unref_5, @function
.type yur_unref_4, @function
.type yur_unref_3, @function
.type yur_unref_2, @function
.type yur_unref_1, @function
yur_unref_6:
.cfi_startproc
  DEC(r9, jmp yur_unref_5)
yur_unref_5:
  DEC(r8, jmp yur_unref_4)
yur_unref_4:
  DEC(rcx, jmp yur_unref_3)
yur_unref_3:
  DEC(rdx, jmp yur_unref_2)
yur_unref_2:
  DEC(rsi, jmp yur_unref_1)
yur_unref_1:
  DEC(rdi, lessstack_ret)
  cmpq %rbp, %rsp
  je __yu_lessstack
  ret
.cfi_endproc


// No need to check for more stack, since there is sufficient red zone stack.
// Note that the function called below is fine because:
//   yur_*unref_children_dealloc: will check for more stack on entry.
#define DEALLOC(deallocator) \
  pushq %rbp; \
  movq %rsp, %rbp; \
  pushq %rdi; \
  pushq %rsi; \
  pushq %rdx; \
  pushq %rcx; \
  pushq %r8; \
  pushq %r9; \
  movq 16(%rbp), %rdi; \
  call deallocator; \
  popq %r9; \
  popq %r8; \
  popq %rcx; \
  popq %rdx; \
  popq %rsi; \
  popq %rdi; \
  movq %rbp, %rsp; \
  popq %rbp; \
  cmpq %rbp, %rsp; \
  je __yu_lessstack; \
  ret


.type pure_dealloc, @function
.p2align 4
pure_dealloc:
.cfi_startproc
  DEALLOC(yur_unref_children_dealloc)
.cfi_endproc


.type destructor_dealloc, @function
.p2align 4
destructor_dealloc:
.cfi_startproc
  DEALLOC(yur_destructor_unref_children_dealloc)
.cfi_endproc
