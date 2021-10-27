
#define yur_NORETURN __attribute__((noreturn))

#define yur_ALWAYS_INLINE inline __attribute__((always_inline))

#define yur_NOINLINE __attribute__((noinline))

#define yur_NAKED __attribute__((naked))

#define yur_LIKELY(x) __builtin_expect(!!(x), 1)

#define yur_UNLIKELY(x) __builtin_expect(!!(x), 0)

#define yur_DO_STRINGIFY(x) #x
#define yur_STRINGIFY(x) yur_DO_STRINGIFY(x)

// The beginning of the current stack segment (growing downward).
// The first (highest) valid segment address is CURR_SEG_BEGIN - 8.
// At the moment CURR_SEG_BEGIN is used for deleting segment,
// and in get_current_segment_size_s. It is possible to get rid of it.

#define CURR_SEG_BEGIN_OFFSET 0x80
#define CURR_SEG_BEGIN %fs:CURR_SEG_BEGIN_OFFSET

// The end of the current stack segment (growing downward).
// This address is the last (lowest) invalid address on the sack segment.

#define CURR_SEG_END_OFFSET 0x70
#define CURR_SEG_END %fs:CURR_SEG_END_OFFSET

// The system stack is saved in TLS while working on the yu stack.

#define SYSTEM_STACK_OFFSET 0x88
#define SYSTEM_STACK %fs:SYSTEM_STACK_OFFSET

// MINIMAL_SEG_RESERVE_SIZE is the minimal amount of reserved
// stack segment space. Currently we need 16 bytes, to have room
// for two return addresses on the stack, and we reserve 224 bytes
// for a more efficient prologue when stack frame is <= 224 bytes.

#define MINIMAL_SEG_RESERVE_SIZE (16 + 224)

// Size of red zone. Amount of bytes assumes to be available for leaf
// functions by gcc, unless -mno-red-zone option is passed to gcc.

#define RED_ZONE_SIZE 128
