# 1 "test.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "test.c"
# 1 "../Frameworks/SDL.framework/Headers/SDL.h" 1
# 72 "../Frameworks/SDL.framework/Headers/SDL.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_main.h" 1
# 25 "../Frameworks/SDL.framework/Headers/SDL_main.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 1
# 31 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_config.h" 1
# 25 "../Frameworks/SDL.framework/Headers/SDL_config.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_platform.h" 1
# 75 "../Frameworks/SDL.framework/Headers/SDL_platform.h"
# 1 "/usr/include/AvailabilityMacros.h" 1 3 4
# 76 "../Frameworks/SDL.framework/Headers/SDL_platform.h" 2
# 1 "/usr/include/TargetConditionals.h" 1 3 4
# 77 "../Frameworks/SDL.framework/Headers/SDL_platform.h" 2
# 128 "../Frameworks/SDL.framework/Headers/SDL_platform.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 129 "../Frameworks/SDL.framework/Headers/SDL_platform.h" 2
# 139 "../Frameworks/SDL.framework/Headers/SDL_platform.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetPlatform (void);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 148 "../Frameworks/SDL.framework/Headers/SDL_platform.h" 2
# 26 "../Frameworks/SDL.framework/Headers/SDL_config.h" 2
# 37 "../Frameworks/SDL.framework/Headers/SDL_config.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_config_macosx.h" 1
# 28 "../Frameworks/SDL.framework/Headers/SDL_config_macosx.h"
# 1 "/usr/include/AvailabilityMacros.h" 1 3 4
# 29 "../Frameworks/SDL.framework/Headers/SDL_config_macosx.h" 2
# 38 "../Frameworks/SDL.framework/Headers/SDL_config.h" 2
# 32 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2



# 1 "/usr/include/sys/types.h" 1 3 4
# 72 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/appleapiopts.h" 1 3 4
# 73 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 417 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/sys/_symbol_aliasing.h" 1 3 4
# 418 "/usr/include/sys/cdefs.h" 2 3 4
# 494 "/usr/include/sys/cdefs.h" 3 4
# 1 "/usr/include/sys/_posix_availability.h" 1 3 4
# 495 "/usr/include/sys/cdefs.h" 2 3 4
# 76 "/usr/include/sys/types.h" 2 3 4


# 1 "/usr/include/machine/types.h" 1 3 4
# 35 "/usr/include/machine/types.h" 3 4
# 1 "/usr/include/i386/types.h" 1 3 4
# 70 "/usr/include/i386/types.h" 3 4
# 1 "/usr/include/i386/_types.h" 1 3 4
# 37 "/usr/include/i386/_types.h" 3 4
typedef signed char __int8_t;



typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;

typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
# 70 "/usr/include/i386/_types.h" 3 4
typedef int __darwin_ct_rune_t;





typedef union {
 char __mbstate8[128];
 long long _mbstateL;
} __mbstate_t;

typedef __mbstate_t __darwin_mbstate_t;


typedef long int __darwin_ptrdiff_t;





typedef long unsigned int __darwin_size_t;





typedef __builtin_va_list __darwin_va_list;





typedef int __darwin_wchar_t;




typedef __darwin_wchar_t __darwin_rune_t;


typedef int __darwin_wint_t;




typedef unsigned long __darwin_clock_t;
typedef __uint32_t __darwin_socklen_t;
typedef long __darwin_ssize_t;
typedef long __darwin_time_t;
# 71 "/usr/include/i386/types.h" 2 3 4







typedef signed char int8_t;

typedef unsigned char u_int8_t;


typedef short int16_t;

typedef unsigned short u_int16_t;


typedef int int32_t;

typedef unsigned int u_int32_t;


typedef long long int64_t;

typedef unsigned long long u_int64_t;


typedef int64_t register_t;






typedef __darwin_intptr_t intptr_t;



typedef unsigned long uintptr_t;




typedef u_int64_t user_addr_t;
typedef u_int64_t user_size_t;
typedef int64_t user_ssize_t;
typedef int64_t user_long_t;
typedef u_int64_t user_ulong_t;
typedef int64_t user_time_t;
typedef int64_t user_off_t;







typedef u_int64_t syscall_arg_t;
# 36 "/usr/include/machine/types.h" 2 3 4
# 79 "/usr/include/sys/types.h" 2 3 4
# 1 "/usr/include/sys/_types.h" 1 3 4
# 33 "/usr/include/sys/_types.h" 3 4
# 1 "/usr/include/machine/_types.h" 1 3 4
# 34 "/usr/include/sys/_types.h" 2 3 4
# 58 "/usr/include/sys/_types.h" 3 4
struct __darwin_pthread_handler_rec
{
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_cond_t { long __sig; char __opaque[40]; };
struct _opaque_pthread_condattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_mutex_t { long __sig; char __opaque[56]; };
struct _opaque_pthread_mutexattr_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_once_t { long __sig; char __opaque[8]; };
struct _opaque_pthread_rwlock_t { long __sig; char __opaque[192]; };
struct _opaque_pthread_rwlockattr_t { long __sig; char __opaque[16]; };
struct _opaque_pthread_t { long __sig; struct __darwin_pthread_handler_rec *__cleanup_stack; char __opaque[1168]; };
# 94 "/usr/include/sys/_types.h" 3 4
typedef __int64_t __darwin_blkcnt_t;
typedef __int32_t __darwin_blksize_t;
typedef __int32_t __darwin_dev_t;
typedef unsigned int __darwin_fsblkcnt_t;
typedef unsigned int __darwin_fsfilcnt_t;
typedef __uint32_t __darwin_gid_t;
typedef __uint32_t __darwin_id_t;
typedef __uint64_t __darwin_ino64_t;

typedef __darwin_ino64_t __darwin_ino_t;



typedef __darwin_natural_t __darwin_mach_port_name_t;
typedef __darwin_mach_port_name_t __darwin_mach_port_t;
typedef __uint16_t __darwin_mode_t;
typedef __int64_t __darwin_off_t;
typedef __int32_t __darwin_pid_t;
typedef struct _opaque_pthread_attr_t
   __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t
   __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t
   __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t
   __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t
   __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t
   __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t
   __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t
   __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t
   *__darwin_pthread_t;
typedef __uint32_t __darwin_sigset_t;
typedef __int32_t __darwin_suseconds_t;
typedef __uint32_t __darwin_uid_t;
typedef __uint32_t __darwin_useconds_t;
typedef unsigned char __darwin_uuid_t[16];
typedef char __darwin_uuid_string_t[37];
# 80 "/usr/include/sys/types.h" 2 3 4

# 1 "/usr/include/machine/endian.h" 1 3 4
# 35 "/usr/include/machine/endian.h" 3 4
# 1 "/usr/include/i386/endian.h" 1 3 4
# 99 "/usr/include/i386/endian.h" 3 4
# 1 "/usr/include/sys/_endian.h" 1 3 4
# 124 "/usr/include/sys/_endian.h" 3 4
# 1 "/usr/include/libkern/_OSByteOrder.h" 1 3 4
# 66 "/usr/include/libkern/_OSByteOrder.h" 3 4
# 1 "/usr/include/libkern/i386/_OSByteOrder.h" 1 3 4
# 44 "/usr/include/libkern/i386/_OSByteOrder.h" 3 4
static __inline__
__uint16_t
_OSSwapInt16(
    __uint16_t _data
)
{
    return ((_data << 8) | (_data >> 8));
}

static __inline__
__uint32_t
_OSSwapInt32(
    __uint32_t _data
)
{

    return __builtin_bswap32(_data);




}


static __inline__
__uint64_t
_OSSwapInt64(
    __uint64_t _data
)
{
    return __builtin_bswap64(_data);
}
# 67 "/usr/include/libkern/_OSByteOrder.h" 2 3 4
# 125 "/usr/include/sys/_endian.h" 2 3 4
# 100 "/usr/include/i386/endian.h" 2 3 4
# 36 "/usr/include/machine/endian.h" 2 3 4
# 82 "/usr/include/sys/types.h" 2 3 4


typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;

typedef unsigned long u_long;


typedef unsigned short ushort;
typedef unsigned int uint;


typedef u_int64_t u_quad_t;
typedef int64_t quad_t;
typedef quad_t * qaddr_t;

typedef char * caddr_t;
typedef int32_t daddr_t;


typedef __darwin_dev_t dev_t;



typedef u_int32_t fixpt_t;


typedef __darwin_blkcnt_t blkcnt_t;




typedef __darwin_blksize_t blksize_t;




typedef __darwin_gid_t gid_t;





typedef __uint32_t in_addr_t;




typedef __uint16_t in_port_t;



typedef __darwin_ino_t ino_t;





typedef __darwin_ino64_t ino64_t;






typedef __int32_t key_t;



typedef __darwin_mode_t mode_t;




typedef __uint16_t nlink_t;





typedef __darwin_id_t id_t;



typedef __darwin_pid_t pid_t;




typedef __darwin_off_t off_t;



typedef int32_t segsz_t;
typedef int32_t swblk_t;


typedef __darwin_uid_t uid_t;
# 223 "/usr/include/sys/types.h" 3 4
typedef __darwin_clock_t clock_t;






typedef __darwin_size_t size_t;




typedef __darwin_ssize_t ssize_t;




typedef __darwin_time_t time_t;




typedef __darwin_useconds_t useconds_t;




typedef __darwin_suseconds_t suseconds_t;
# 260 "/usr/include/sys/types.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 183 "/usr/include/sys/_structs.h" 3 4

typedef struct fd_set {
 __int32_t fds_bits[((((1024) % ((sizeof(__int32_t) * 8))) == 0) ? ((1024) / ((sizeof(__int32_t) * 8))) : (((1024) / ((sizeof(__int32_t) * 8))) + 1))];
} fd_set;



static __inline int
__darwin_fd_isset(int _n, const struct fd_set *_p)
{
 return (_p->fds_bits[_n/(sizeof(__int32_t) * 8)] & (1<<(_n % (sizeof(__int32_t) * 8))));
}
# 261 "/usr/include/sys/types.h" 2 3 4




typedef __int32_t fd_mask;
# 318 "/usr/include/sys/types.h" 3 4
typedef __darwin_pthread_attr_t pthread_attr_t;



typedef __darwin_pthread_cond_t pthread_cond_t;



typedef __darwin_pthread_condattr_t pthread_condattr_t;



typedef __darwin_pthread_mutex_t pthread_mutex_t;



typedef __darwin_pthread_mutexattr_t pthread_mutexattr_t;



typedef __darwin_pthread_once_t pthread_once_t;



typedef __darwin_pthread_rwlock_t pthread_rwlock_t;



typedef __darwin_pthread_rwlockattr_t pthread_rwlockattr_t;



typedef __darwin_pthread_t pthread_t;






typedef __darwin_pthread_key_t pthread_key_t;





typedef __darwin_fsblkcnt_t fsblkcnt_t;




typedef __darwin_fsfilcnt_t fsfilcnt_t;
# 36 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2


# 1 "/usr/include/stdio.h" 1 3 4
# 65 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/Availability.h" 1 3 4
# 141 "/usr/include/Availability.h" 3 4
# 1 "/usr/include/AvailabilityInternal.h" 1 3 4
# 142 "/usr/include/Availability.h" 2 3 4
# 66 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/_types.h" 1 3 4
# 39 "/usr/include/_types.h" 3 4
typedef int __darwin_nl_item;
typedef int __darwin_wctrans_t;

typedef __uint32_t __darwin_wctype_t;
# 68 "/usr/include/stdio.h" 2 3 4





typedef __darwin_va_list va_list;
# 85 "/usr/include/stdio.h" 3 4
typedef __darwin_off_t fpos_t;
# 96 "/usr/include/stdio.h" 3 4
struct __sbuf {
 unsigned char *_base;
 int _size;
};


struct __sFILEX;
# 130 "/usr/include/stdio.h" 3 4
typedef struct __sFILE {
 unsigned char *_p;
 int _r;
 int _w;
 short _flags;
 short _file;
 struct __sbuf _bf;
 int _lbfsize;


 void *_cookie;
 int (*_close)(void *);
 int (*_read) (void *, char *, int);
 fpos_t (*_seek) (void *, fpos_t, int);
 int (*_write)(void *, const char *, int);


 struct __sbuf _ub;
 struct __sFILEX *_extra;
 int _ur;


 unsigned char _ubuf[3];
 unsigned char _nbuf[1];


 struct __sbuf _lb;


 int _blksize;
 fpos_t _offset;
} FILE;


extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;

# 238 "/usr/include/stdio.h" 3 4

void clearerr(FILE *);
int fclose(FILE *);
int feof(FILE *);
int ferror(FILE *);
int fflush(FILE *);
int fgetc(FILE *);
int fgetpos(FILE * , fpos_t *);
char *fgets(char * , int, FILE *);



FILE *fopen(const char * , const char * ) __asm("_" "fopen" );

int fprintf(FILE * , const char * , ...) __attribute__((__format__ (__printf__, 2, 3)));
int fputc(int, FILE *);
int fputs(const char * , FILE * ) __asm("_" "fputs" );
size_t fread(void * , size_t, size_t, FILE * );
FILE *freopen(const char * , const char * ,
                 FILE * ) __asm("_" "freopen" );
int fscanf(FILE * , const char * , ...) __attribute__((__format__ (__scanf__, 2, 3)));
int fseek(FILE *, long, int);
int fsetpos(FILE *, const fpos_t *);
long ftell(FILE *);
size_t fwrite(const void * , size_t, size_t, FILE * ) __asm("_" "fwrite" );
int getc(FILE *);
int getchar(void);
char *gets(char *);
void perror(const char *);
int printf(const char * , ...) __attribute__((__format__ (__printf__, 1, 2)));
int putc(int, FILE *);
int putchar(int);
int puts(const char *);
int remove(const char *);
int rename (const char *, const char *);
void rewind(FILE *);
int scanf(const char * , ...) __attribute__((__format__ (__scanf__, 1, 2)));
void setbuf(FILE * , char * );
int setvbuf(FILE * , char * , int, size_t);
int sprintf(char * , const char * , ...) __attribute__((__format__ (__printf__, 2, 3)));
int sscanf(const char * , const char * , ...) __attribute__((__format__ (__scanf__, 2, 3)));
FILE *tmpfile(void);
char *tmpnam(char *);
int ungetc(int, FILE *);
int vfprintf(FILE * , const char * , va_list) __attribute__((__format__ (__printf__, 2, 0)));
int vprintf(const char * , va_list) __attribute__((__format__ (__printf__, 1, 0)));
int vsprintf(char * , const char * , va_list) __attribute__((__format__ (__printf__, 2, 0)));

# 296 "/usr/include/stdio.h" 3 4




char *ctermid(char *);





FILE *fdopen(int, const char *) __asm("_" "fdopen" );

int fileno(FILE *);

# 318 "/usr/include/stdio.h" 3 4

int pclose(FILE *);



FILE *popen(const char *, const char *) __asm("_" "popen" );


# 340 "/usr/include/stdio.h" 3 4

int __srget(FILE *);
int __svfscanf(FILE *, const char *, va_list) __attribute__((__format__ (__scanf__, 2, 0)));
int __swbuf(int, FILE *);








static __inline int __sputc(int _c, FILE *_p) {
 if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
  return (*_p->_p++ = _c);
 else
  return (__swbuf(_c, _p));
}
# 377 "/usr/include/stdio.h" 3 4

void flockfile(FILE *);
int ftrylockfile(FILE *);
void funlockfile(FILE *);
int getc_unlocked(FILE *);
int getchar_unlocked(void);
int putc_unlocked(int, FILE *);
int putchar_unlocked(int);



int getw(FILE *);
int putw(int, FILE *);


char *tempnam(const char *, const char *) __asm("_" "tempnam" );

# 417 "/usr/include/stdio.h" 3 4

int fseeko(FILE *, off_t, int);
off_t ftello(FILE *);





int snprintf(char * , size_t, const char * , ...) __attribute__((__format__ (__printf__, 3, 4)));
int vfscanf(FILE * , const char * , va_list) __attribute__((__format__ (__scanf__, 2, 0)));
int vscanf(const char * , va_list) __attribute__((__format__ (__scanf__, 1, 0)));
int vsnprintf(char * , size_t, const char * , va_list) __attribute__((__format__ (__printf__, 3, 0)));
int vsscanf(const char * , const char * , va_list) __attribute__((__format__ (__scanf__, 2, 0)));

# 445 "/usr/include/stdio.h" 3 4

int dprintf(int, const char * , ...) __attribute__((__format__ (__printf__, 2, 3))) __attribute__((visibility("default")));
int vdprintf(int, const char * , va_list) __attribute__((__format__ (__printf__, 2, 0))) __attribute__((visibility("default")));
ssize_t getdelim(char ** , size_t * , int, FILE * ) __attribute__((visibility("default")));
ssize_t getline(char ** , size_t * , FILE * ) __attribute__((visibility("default")));









extern const int sys_nerr;
extern const char *const sys_errlist[];

int asprintf(char **, const char *, ...) __attribute__((__format__ (__printf__, 2, 3)));
char *ctermid_r(char *);
char *fgetln(FILE *, size_t *);
const char *fmtcheck(const char *, const char *);
int fpurge(FILE *);
void setbuffer(FILE *, char *, int);
int setlinebuf(FILE *);
int vasprintf(char **, const char *, va_list) __attribute__((__format__ (__printf__, 2, 0)));
FILE *zopen(const char *, const char *, int);





FILE *funopen(const void *,
                 int (*)(void *, char *, int),
                 int (*)(void *, const char *, int),
                 fpos_t (*)(void *, fpos_t, int),
                 int (*)(void *));

# 499 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/secure/_stdio.h" 1 3 4
# 31 "/usr/include/secure/_stdio.h" 3 4
# 1 "/usr/include/secure/_common.h" 1 3 4
# 32 "/usr/include/secure/_stdio.h" 2 3 4
# 45 "/usr/include/secure/_stdio.h" 3 4
extern int __sprintf_chk (char * , int, size_t,
     const char * , ...)
  ;




extern int __snprintf_chk (char * , size_t, int, size_t,
      const char * , ...)
  ;





extern int __vsprintf_chk (char * , int, size_t,
      const char * , va_list)
  ;




extern int __vsnprintf_chk (char * , size_t, int, size_t,
       const char * , va_list)
  ;
# 500 "/usr/include/stdio.h" 2 3 4
# 39 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2


# 1 "/usr/include/stdlib.h" 1 3 4
# 65 "/usr/include/stdlib.h" 3 4
# 1 "/usr/include/sys/wait.h" 1 3 4
# 79 "/usr/include/sys/wait.h" 3 4
typedef enum {
 P_ALL,
 P_PID,
 P_PGID
} idtype_t;
# 116 "/usr/include/sys/wait.h" 3 4
# 1 "/usr/include/sys/signal.h" 1 3 4
# 81 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/machine/signal.h" 1 3 4
# 32 "/usr/include/machine/signal.h" 3 4
# 1 "/usr/include/i386/signal.h" 1 3 4
# 39 "/usr/include/i386/signal.h" 3 4
typedef int sig_atomic_t;
# 55 "/usr/include/i386/signal.h" 3 4
# 1 "/usr/include/i386/_structs.h" 1 3 4
# 56 "/usr/include/i386/signal.h" 2 3 4
# 33 "/usr/include/machine/signal.h" 2 3 4
# 82 "/usr/include/sys/signal.h" 2 3 4
# 148 "/usr/include/sys/signal.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 57 "/usr/include/sys/_structs.h" 3 4
# 1 "/usr/include/machine/_structs.h" 1 3 4
# 29 "/usr/include/machine/_structs.h" 3 4
# 1 "/usr/include/i386/_structs.h" 1 3 4
# 38 "/usr/include/i386/_structs.h" 3 4
# 1 "/usr/include/mach/i386/_structs.h" 1 3 4
# 43 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_thread_state
{
    unsigned int __eax;
    unsigned int __ebx;
    unsigned int __ecx;
    unsigned int __edx;
    unsigned int __edi;
    unsigned int __esi;
    unsigned int __ebp;
    unsigned int __esp;
    unsigned int __ss;
    unsigned int __eflags;
    unsigned int __eip;
    unsigned int __cs;
    unsigned int __ds;
    unsigned int __es;
    unsigned int __fs;
    unsigned int __gs;
};
# 89 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_fp_control
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
      :2,
    __pc :2,





    __rc :2,






             :1,
      :3;
};
typedef struct __darwin_fp_control __darwin_fp_control_t;
# 147 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_fp_status
{
    unsigned short __invalid :1,
        __denorm :1,
    __zdiv :1,
    __ovrfl :1,
    __undfl :1,
    __precis :1,
    __stkflt :1,
    __errsumm :1,
    __c0 :1,
    __c1 :1,
    __c2 :1,
    __tos :3,
    __c3 :1,
    __busy :1;
};
typedef struct __darwin_fp_status __darwin_fp_status_t;
# 191 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_mmst_reg
{
 char __mmst_reg[10];
 char __mmst_rsrv[6];
};
# 210 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_xmm_reg
{
 char __xmm_reg[16];
};
# 232 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_float_state
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;
 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;
 __uint16_t __fpu_rsrv2;
 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;
 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 char __fpu_rsrv4[14*16];
 int __fpu_reserved1;
};


struct __darwin_i386_avx_state
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;
 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;
 __uint16_t __fpu_rsrv2;
 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;
 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 char __fpu_rsrv4[14*16];
 int __fpu_reserved1;
 char __avx_reserved1[64];
 struct __darwin_xmm_reg __fpu_ymmh0;
 struct __darwin_xmm_reg __fpu_ymmh1;
 struct __darwin_xmm_reg __fpu_ymmh2;
 struct __darwin_xmm_reg __fpu_ymmh3;
 struct __darwin_xmm_reg __fpu_ymmh4;
 struct __darwin_xmm_reg __fpu_ymmh5;
 struct __darwin_xmm_reg __fpu_ymmh6;
 struct __darwin_xmm_reg __fpu_ymmh7;
};
# 402 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_i386_exception_state
{
 __uint16_t __trapno;
 __uint16_t __cpu;
 __uint32_t __err;
 __uint32_t __faultvaddr;
};
# 422 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_debug_state32
{
 unsigned int __dr0;
 unsigned int __dr1;
 unsigned int __dr2;
 unsigned int __dr3;
 unsigned int __dr4;
 unsigned int __dr5;
 unsigned int __dr6;
 unsigned int __dr7;
};
# 454 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_thread_state64
{
 __uint64_t __rax;
 __uint64_t __rbx;
 __uint64_t __rcx;
 __uint64_t __rdx;
 __uint64_t __rdi;
 __uint64_t __rsi;
 __uint64_t __rbp;
 __uint64_t __rsp;
 __uint64_t __r8;
 __uint64_t __r9;
 __uint64_t __r10;
 __uint64_t __r11;
 __uint64_t __r12;
 __uint64_t __r13;
 __uint64_t __r14;
 __uint64_t __r15;
 __uint64_t __rip;
 __uint64_t __rflags;
 __uint64_t __cs;
 __uint64_t __fs;
 __uint64_t __gs;
};
# 509 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_float_state64
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;


 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;

 __uint16_t __fpu_rsrv2;


 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;

 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 struct __darwin_xmm_reg __fpu_xmm8;
 struct __darwin_xmm_reg __fpu_xmm9;
 struct __darwin_xmm_reg __fpu_xmm10;
 struct __darwin_xmm_reg __fpu_xmm11;
 struct __darwin_xmm_reg __fpu_xmm12;
 struct __darwin_xmm_reg __fpu_xmm13;
 struct __darwin_xmm_reg __fpu_xmm14;
 struct __darwin_xmm_reg __fpu_xmm15;
 char __fpu_rsrv4[6*16];
 int __fpu_reserved1;
};


struct __darwin_x86_avx_state64
{
 int __fpu_reserved[2];
 struct __darwin_fp_control __fpu_fcw;
 struct __darwin_fp_status __fpu_fsw;
 __uint8_t __fpu_ftw;
 __uint8_t __fpu_rsrv1;
 __uint16_t __fpu_fop;


 __uint32_t __fpu_ip;
 __uint16_t __fpu_cs;

 __uint16_t __fpu_rsrv2;


 __uint32_t __fpu_dp;
 __uint16_t __fpu_ds;

 __uint16_t __fpu_rsrv3;
 __uint32_t __fpu_mxcsr;
 __uint32_t __fpu_mxcsrmask;
 struct __darwin_mmst_reg __fpu_stmm0;
 struct __darwin_mmst_reg __fpu_stmm1;
 struct __darwin_mmst_reg __fpu_stmm2;
 struct __darwin_mmst_reg __fpu_stmm3;
 struct __darwin_mmst_reg __fpu_stmm4;
 struct __darwin_mmst_reg __fpu_stmm5;
 struct __darwin_mmst_reg __fpu_stmm6;
 struct __darwin_mmst_reg __fpu_stmm7;
 struct __darwin_xmm_reg __fpu_xmm0;
 struct __darwin_xmm_reg __fpu_xmm1;
 struct __darwin_xmm_reg __fpu_xmm2;
 struct __darwin_xmm_reg __fpu_xmm3;
 struct __darwin_xmm_reg __fpu_xmm4;
 struct __darwin_xmm_reg __fpu_xmm5;
 struct __darwin_xmm_reg __fpu_xmm6;
 struct __darwin_xmm_reg __fpu_xmm7;
 struct __darwin_xmm_reg __fpu_xmm8;
 struct __darwin_xmm_reg __fpu_xmm9;
 struct __darwin_xmm_reg __fpu_xmm10;
 struct __darwin_xmm_reg __fpu_xmm11;
 struct __darwin_xmm_reg __fpu_xmm12;
 struct __darwin_xmm_reg __fpu_xmm13;
 struct __darwin_xmm_reg __fpu_xmm14;
 struct __darwin_xmm_reg __fpu_xmm15;
 char __fpu_rsrv4[6*16];
 int __fpu_reserved1;
 char __avx_reserved1[64];
 struct __darwin_xmm_reg __fpu_ymmh0;
 struct __darwin_xmm_reg __fpu_ymmh1;
 struct __darwin_xmm_reg __fpu_ymmh2;
 struct __darwin_xmm_reg __fpu_ymmh3;
 struct __darwin_xmm_reg __fpu_ymmh4;
 struct __darwin_xmm_reg __fpu_ymmh5;
 struct __darwin_xmm_reg __fpu_ymmh6;
 struct __darwin_xmm_reg __fpu_ymmh7;
 struct __darwin_xmm_reg __fpu_ymmh8;
 struct __darwin_xmm_reg __fpu_ymmh9;
 struct __darwin_xmm_reg __fpu_ymmh10;
 struct __darwin_xmm_reg __fpu_ymmh11;
 struct __darwin_xmm_reg __fpu_ymmh12;
 struct __darwin_xmm_reg __fpu_ymmh13;
 struct __darwin_xmm_reg __fpu_ymmh14;
 struct __darwin_xmm_reg __fpu_ymmh15;
};
# 751 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_exception_state64
{
    __uint16_t __trapno;
    __uint16_t __cpu;
    __uint32_t __err;
    __uint64_t __faultvaddr;
};
# 771 "/usr/include/mach/i386/_structs.h" 3 4
struct __darwin_x86_debug_state64
{
 __uint64_t __dr0;
 __uint64_t __dr1;
 __uint64_t __dr2;
 __uint64_t __dr3;
 __uint64_t __dr4;
 __uint64_t __dr5;
 __uint64_t __dr6;
 __uint64_t __dr7;
};
# 39 "/usr/include/i386/_structs.h" 2 3 4
# 48 "/usr/include/i386/_structs.h" 3 4
struct __darwin_mcontext32
{
 struct __darwin_i386_exception_state __es;
 struct __darwin_i386_thread_state __ss;
 struct __darwin_i386_float_state __fs;
};


struct __darwin_mcontext_avx32
{
 struct __darwin_i386_exception_state __es;
 struct __darwin_i386_thread_state __ss;
 struct __darwin_i386_avx_state __fs;
};
# 86 "/usr/include/i386/_structs.h" 3 4
struct __darwin_mcontext64
{
 struct __darwin_x86_exception_state64 __es;
 struct __darwin_x86_thread_state64 __ss;
 struct __darwin_x86_float_state64 __fs;
};


struct __darwin_mcontext_avx64
{
 struct __darwin_x86_exception_state64 __es;
 struct __darwin_x86_thread_state64 __ss;
 struct __darwin_x86_avx_state64 __fs;
};
# 127 "/usr/include/i386/_structs.h" 3 4
typedef struct __darwin_mcontext64 *mcontext_t;
# 30 "/usr/include/machine/_structs.h" 2 3 4
# 58 "/usr/include/sys/_structs.h" 2 3 4
# 75 "/usr/include/sys/_structs.h" 3 4
struct __darwin_sigaltstack
{
 void *ss_sp;
 __darwin_size_t ss_size;
 int ss_flags;
};
# 128 "/usr/include/sys/_structs.h" 3 4
struct __darwin_ucontext
{
 int uc_onstack;
 __darwin_sigset_t uc_sigmask;
 struct __darwin_sigaltstack uc_stack;
 struct __darwin_ucontext *uc_link;
 __darwin_size_t uc_mcsize;
 struct __darwin_mcontext64 *uc_mcontext;



};
# 218 "/usr/include/sys/_structs.h" 3 4
typedef struct __darwin_sigaltstack stack_t;
# 227 "/usr/include/sys/_structs.h" 3 4
typedef struct __darwin_ucontext ucontext_t;
# 149 "/usr/include/sys/signal.h" 2 3 4
# 162 "/usr/include/sys/signal.h" 3 4
typedef __darwin_sigset_t sigset_t;
# 175 "/usr/include/sys/signal.h" 3 4
union sigval {

 int sival_int;
 void *sival_ptr;
};





struct sigevent {
 int sigev_notify;
 int sigev_signo;
 union sigval sigev_value;
 void (*sigev_notify_function)(union sigval);
 pthread_attr_t *sigev_notify_attributes;
};


typedef struct __siginfo {
 int si_signo;
 int si_errno;
 int si_code;
 pid_t si_pid;
 uid_t si_uid;
 int si_status;
 void *si_addr;
 union sigval si_value;
 long si_band;
 unsigned long __pad[7];
} siginfo_t;
# 286 "/usr/include/sys/signal.h" 3 4
union __sigaction_u {
 void (*__sa_handler)(int);
 void (*__sa_sigaction)(int, struct __siginfo *,
         void *);
};


struct __sigaction {
 union __sigaction_u __sigaction_u;
 void (*sa_tramp)(void *, int, int, siginfo_t *, void *);
 sigset_t sa_mask;
 int sa_flags;
};




struct sigaction {
 union __sigaction_u __sigaction_u;
 sigset_t sa_mask;
 int sa_flags;
};
# 348 "/usr/include/sys/signal.h" 3 4
typedef void (*sig_t)(int);
# 365 "/usr/include/sys/signal.h" 3 4
struct sigvec {
 void (*sv_handler)(int);
 int sv_mask;
 int sv_flags;
};
# 384 "/usr/include/sys/signal.h" 3 4
struct sigstack {
 char *ss_sp;
 int ss_onstack;
};
# 406 "/usr/include/sys/signal.h" 3 4

void (*signal(int, void (*)(int)))(int);

# 117 "/usr/include/sys/wait.h" 2 3 4
# 1 "/usr/include/sys/resource.h" 1 3 4
# 77 "/usr/include/sys/resource.h" 3 4
# 1 "/usr/include/sys/_structs.h" 1 3 4
# 100 "/usr/include/sys/_structs.h" 3 4
struct timeval
{
 __darwin_time_t tv_sec;
 __darwin_suseconds_t tv_usec;
};
# 78 "/usr/include/sys/resource.h" 2 3 4
# 89 "/usr/include/sys/resource.h" 3 4
typedef __uint64_t rlim_t;
# 151 "/usr/include/sys/resource.h" 3 4
struct rusage {
 struct timeval ru_utime;
 struct timeval ru_stime;
# 162 "/usr/include/sys/resource.h" 3 4
 long ru_maxrss;

 long ru_ixrss;
 long ru_idrss;
 long ru_isrss;
 long ru_minflt;
 long ru_majflt;
 long ru_nswap;
 long ru_inblock;
 long ru_oublock;
 long ru_msgsnd;
 long ru_msgrcv;
 long ru_nsignals;
 long ru_nvcsw;
 long ru_nivcsw;


};
# 222 "/usr/include/sys/resource.h" 3 4
struct rlimit {
 rlim_t rlim_cur;
 rlim_t rlim_max;
};
# 244 "/usr/include/sys/resource.h" 3 4

int getpriority(int, id_t);

int getiopolicy_np(int, int) __attribute__((visibility("default")));

int getrlimit(int, struct rlimit *) __asm("_" "getrlimit" );
int getrusage(int, struct rusage *);
int setpriority(int, id_t, int);

int setiopolicy_np(int, int, int) __attribute__((visibility("default")));

int setrlimit(int, const struct rlimit *) __asm("_" "setrlimit" );

# 118 "/usr/include/sys/wait.h" 2 3 4
# 201 "/usr/include/sys/wait.h" 3 4
union wait {
 int w_status;



 struct {

  unsigned int w_Termsig:7,
    w_Coredump:1,
    w_Retcode:8,
    w_Filler:16;







 } w_T;





 struct {

  unsigned int w_Stopval:8,
    w_Stopsig:8,
    w_Filler:16;






 } w_S;
};
# 254 "/usr/include/sys/wait.h" 3 4

pid_t wait(int *) __asm("_" "wait" );
pid_t waitpid(pid_t, int *, int) __asm("_" "waitpid" );

int waitid(idtype_t, id_t, siginfo_t *, int) __asm("_" "waitid" );


pid_t wait3(int *, int, struct rusage *);
pid_t wait4(pid_t, int *, int, struct rusage *);


# 66 "/usr/include/stdlib.h" 2 3 4

# 1 "/usr/include/alloca.h" 1 3 4
# 35 "/usr/include/alloca.h" 3 4

void *alloca(size_t);

# 68 "/usr/include/stdlib.h" 2 3 4
# 81 "/usr/include/stdlib.h" 3 4
typedef __darwin_ct_rune_t ct_rune_t;




typedef __darwin_rune_t rune_t;






typedef __darwin_wchar_t wchar_t;



typedef struct {
 int quot;
 int rem;
} div_t;

typedef struct {
 long quot;
 long rem;
} ldiv_t;


typedef struct {
 long long quot;
 long long rem;
} lldiv_t;
# 134 "/usr/include/stdlib.h" 3 4
extern int __mb_cur_max;
# 144 "/usr/include/stdlib.h" 3 4

void abort(void) __attribute__((__noreturn__));
int abs(int) __attribute__((__const__));
int atexit(void (*)(void));
double atof(const char *);
int atoi(const char *);
long atol(const char *);

long long
  atoll(const char *);

void *bsearch(const void *, const void *, size_t,
     size_t, int (*)(const void *, const void *));
void *calloc(size_t, size_t);
div_t div(int, int) __attribute__((__const__));
void exit(int) __attribute__((__noreturn__));
void free(void *);
char *getenv(const char *);
long labs(long) __attribute__((__const__));
ldiv_t ldiv(long, long) __attribute__((__const__));

long long
  llabs(long long);
lldiv_t lldiv(long long, long long);

void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t * , const char * , size_t);
int mbtowc(wchar_t * , const char * , size_t);
int posix_memalign(void **, size_t, size_t) __attribute__((visibility("default")));
void qsort(void *, size_t, size_t,
     int (*)(const void *, const void *));
int rand(void);
void *realloc(void *, size_t);
void srand(unsigned);
double strtod(const char *, char **) __asm("_" "strtod" );
float strtof(const char *, char **) __asm("_" "strtof" );
long strtol(const char *, char **, int);
long double
  strtold(const char *, char **) ;

long long
  strtoll(const char *, char **, int);

unsigned long
  strtoul(const char *, char **, int);

unsigned long long
  strtoull(const char *, char **, int);

int system(const char *) __asm("_" "system" );
size_t wcstombs(char * , const wchar_t * , size_t);
int wctomb(char *, wchar_t);


void _Exit(int) __attribute__((__noreturn__));
long a64l(const char *);
double drand48(void);
char *ecvt(double, int, int *, int *);
double erand48(unsigned short[3]);
char *fcvt(double, int, int *, int *);
char *gcvt(double, int, char *);
int getsubopt(char **, char * const *, char **);
int grantpt(int);

char *initstate(unsigned, char *, size_t);



long jrand48(unsigned short[3]);
char *l64a(long);
void lcong48(unsigned short[7]);
long lrand48(void);
char *mktemp(char *);
int mkstemp(char *);
long mrand48(void);
long nrand48(unsigned short[3]);
int posix_openpt(int);
char *ptsname(int);
int putenv(char *) __asm("_" "putenv" );
long random(void);
int rand_r(unsigned *);

char *realpath(const char * , char * ) __asm("_" "realpath" "$DARWIN_EXTSN");



unsigned short
 *seed48(unsigned short[3]);
int setenv(const char *, const char *, int) __asm("_" "setenv" );

void setkey(const char *) __asm("_" "setkey" );



char *setstate(const char *);
void srand48(long);

void srandom(unsigned);



int unlockpt(int);

int unsetenv(const char *) __asm("_" "unsetenv" );
# 267 "/usr/include/stdlib.h" 3 4
u_int32_t
  arc4random(void);
void arc4random_addrandom(unsigned char * , int );
void arc4random_buf(void * , size_t ) __attribute__((visibility("default")));
void arc4random_stir(void);
u_int32_t
  arc4random_uniform(u_int32_t ) __attribute__((visibility("default")));

int atexit_b(void (^)(void)) __attribute__((visibility("default")));
void *bsearch_b(const void *, const void *, size_t,
     size_t, int (^)(const void *, const void *)) __attribute__((visibility("default")));



char *cgetcap(char *, const char *, int);
int cgetclose(void);
int cgetent(char **, char **, const char *);
int cgetfirst(char **, char **);
int cgetmatch(const char *, const char *);
int cgetnext(char **, char **);
int cgetnum(char *, const char *, long *);
int cgetset(const char *);
int cgetstr(char *, const char *, char **);
int cgetustr(char *, const char *, char **);

int daemon(int, int) __asm("_" "daemon" "$1050") __attribute__((deprecated,visibility("default")));
char *devname(dev_t, mode_t);
char *devname_r(dev_t, mode_t, char *buf, int len);
char *getbsize(int *, long *);
int getloadavg(double [], int);
const char
 *getprogname(void);

int heapsort(void *, size_t, size_t,
     int (*)(const void *, const void *));

int heapsort_b(void *, size_t, size_t,
     int (^)(const void *, const void *)) __attribute__((visibility("default")));

int mergesort(void *, size_t, size_t,
     int (*)(const void *, const void *));

int mergesort_b(void *, size_t, size_t,
     int (^)(const void *, const void *)) __attribute__((visibility("default")));

void psort(void *, size_t, size_t,
     int (*)(const void *, const void *)) __attribute__((visibility("default")));

void psort_b(void *, size_t, size_t,
     int (^)(const void *, const void *)) __attribute__((visibility("default")));

void psort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *)) __attribute__((visibility("default")));

void qsort_b(void *, size_t, size_t,
     int (^)(const void *, const void *)) __attribute__((visibility("default")));

void qsort_r(void *, size_t, size_t, void *,
     int (*)(void *, const void *, const void *));
int radixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void setprogname(const char *);
int sradixsort(const unsigned char **, int, const unsigned char *,
     unsigned);
void sranddev(void);
void srandomdev(void);
void *reallocf(void *, size_t);

long long
  strtoq(const char *, char **, int);
unsigned long long
  strtouq(const char *, char **, int);

extern char *suboptarg;
void *valloc(size_t);







# 42 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stddef.h" 1 3 4
# 152 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 43 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stdarg.h" 1 3 4
# 43 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 44 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 61 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
# 1 "/usr/include/string.h" 1 3 4
# 79 "/usr/include/string.h" 3 4

void *memchr(const void *, int, size_t);
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memmove(void *, const void *, size_t);
void *memset(void *, int, size_t);
char *strcat(char *, const char *);
char *strchr(const char *, int);
int strcmp(const char *, const char *);
int strcoll(const char *, const char *);
char *strcpy(char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int) __asm("_" "strerror" );
size_t strlen(const char *);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
size_t strspn(const char *, const char *);
char *strstr(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);

# 113 "/usr/include/string.h" 3 4

char *strtok_r(char *, const char *, char **);

# 125 "/usr/include/string.h" 3 4

int strerror_r(int, char *, size_t);
char *strdup(const char *);
void *memccpy(void *, const void *, int, size_t);

# 139 "/usr/include/string.h" 3 4

char *stpcpy(char *, const char *);
char *stpncpy(char *, const char *, size_t) __attribute__((visibility("default")));
char *strndup(const char *, size_t) __attribute__((visibility("default")));
size_t strnlen(const char *, size_t) __attribute__((visibility("default")));
char *strsignal(int sig);

# 158 "/usr/include/string.h" 3 4

void *memmem(const void *, size_t, const void *, size_t) __attribute__((visibility("default")));
void memset_pattern4(void *, const void *, size_t) __attribute__((visibility("default")));
void memset_pattern8(void *, const void *, size_t) __attribute__((visibility("default")));
void memset_pattern16(void *, const void *, size_t) __attribute__((visibility("default")));

char *strcasestr(const char *, const char *);
char *strnstr(const char *, const char *, size_t);
size_t strlcat(char *, const char *, size_t);
size_t strlcpy(char *, const char *, size_t);
void strmode(int, char *);
char *strsep(char **, const char *);


void swab(const void * , void * , ssize_t);







# 1 "/usr/include/strings.h" 1 3 4
# 71 "/usr/include/strings.h" 3 4



int bcmp(const void *, const void *, size_t) ;
void bcopy(const void *, void *, size_t) ;
void bzero(void *, size_t) ;
char *index(const char *, int) ;
char *rindex(const char *, int) ;


int ffs(int);
int strcasecmp(const char *, const char *);
int strncasecmp(const char *, const char *, size_t);





int ffsl(long) __attribute__((visibility("default")));
int fls(int) __attribute__((visibility("default")));
int flsl(long) __attribute__((visibility("default")));


# 1 "/usr/include/string.h" 1 3 4
# 95 "/usr/include/strings.h" 2 3 4
# 181 "/usr/include/string.h" 2 3 4
# 190 "/usr/include/string.h" 3 4
# 1 "/usr/include/secure/_string.h" 1 3 4
# 58 "/usr/include/secure/_string.h" 3 4
static __inline void *
__inline_memcpy_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memcpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}






static __inline void *
__inline_memmove_chk (void *__dest, const void *__src, size_t __len)
{
  return __builtin___memmove_chk (__dest, __src, __len, __builtin_object_size (__dest, 0));
}






static __inline void *
__inline_memset_chk (void *__dest, int __val, size_t __len)
{
  return __builtin___memset_chk (__dest, __val, __len, __builtin_object_size (__dest, 0));
}






static __inline char *
__inline_strcpy_chk (char * __dest, const char * __src)
{
  return __builtin___strcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}







static __inline char *
__inline_stpcpy_chk (char *__dest, const char *__src)
{
  return __builtin___stpcpy_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}






static __inline char *
__inline_stpncpy_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___stpncpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}







static __inline char *
__inline_strncpy_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncpy_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}






static __inline char *
__inline_strcat_chk (char * __dest, const char * __src)
{
  return __builtin___strcat_chk (__dest, __src, __builtin_object_size (__dest, 2 > 1));
}







static __inline char *
__inline_strncat_chk (char * __dest, const char * __src,
        size_t __len)
{
  return __builtin___strncat_chk (__dest, __src, __len, __builtin_object_size (__dest, 2 > 1));
}
# 191 "/usr/include/string.h" 2 3 4
# 62 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2





# 1 "/usr/include/inttypes.h" 1 3 4
# 247 "/usr/include/inttypes.h" 3 4
# 1 "/usr/include/stdint.h" 1 3 4
# 40 "/usr/include/stdint.h" 3 4
typedef unsigned char uint8_t;




typedef unsigned short uint16_t;




typedef unsigned int uint32_t;




typedef unsigned long long uint64_t;



typedef int8_t int_least8_t;
typedef int16_t int_least16_t;
typedef int32_t int_least32_t;
typedef int64_t int_least64_t;
typedef uint8_t uint_least8_t;
typedef uint16_t uint_least16_t;
typedef uint32_t uint_least32_t;
typedef uint64_t uint_least64_t;



typedef int8_t int_fast8_t;
typedef int16_t int_fast16_t;
typedef int32_t int_fast32_t;
typedef int64_t int_fast64_t;
typedef uint8_t uint_fast8_t;
typedef uint16_t uint_fast16_t;
typedef uint32_t uint_fast32_t;
typedef uint64_t uint_fast64_t;
# 97 "/usr/include/stdint.h" 3 4
typedef long int intmax_t;
# 106 "/usr/include/stdint.h" 3 4
typedef long unsigned int uintmax_t;
# 248 "/usr/include/inttypes.h" 2 3 4
# 257 "/usr/include/inttypes.h" 3 4



  extern intmax_t imaxabs(intmax_t j);


  typedef struct {
        intmax_t quot;
        intmax_t rem;
  } imaxdiv_t;

  extern imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom);


  extern intmax_t strtoimax(const char * nptr, char ** endptr, int base);
  extern uintmax_t strtoumax(const char * nptr, char ** endptr, int base);
# 282 "/usr/include/inttypes.h" 3 4
  extern intmax_t wcstoimax(const wchar_t * nptr, wchar_t ** endptr, int base);
  extern uintmax_t wcstoumax(const wchar_t * nptr, wchar_t ** endptr, int base);







# 68 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2




# 1 "/usr/include/ctype.h" 1 3 4
# 69 "/usr/include/ctype.h" 3 4
# 1 "/usr/include/runetype.h" 1 3 4
# 70 "/usr/include/runetype.h" 3 4
typedef __darwin_wint_t wint_t;
# 81 "/usr/include/runetype.h" 3 4
typedef struct {
 __darwin_rune_t __min;
 __darwin_rune_t __max;
 __darwin_rune_t __map;
 __uint32_t *__types;
} _RuneEntry;

typedef struct {
 int __nranges;
 _RuneEntry *__ranges;
} _RuneRange;

typedef struct {
 char __name[14];
 __uint32_t __mask;
} _RuneCharClass;

typedef struct {
 char __magic[8];
 char __encoding[32];

 __darwin_rune_t (*__sgetrune)(const char *, __darwin_size_t, char const **);
 int (*__sputrune)(__darwin_rune_t, char *, __darwin_size_t, char **);
 __darwin_rune_t __invalid_rune;

 __uint32_t __runetype[(1 <<8 )];
 __darwin_rune_t __maplower[(1 <<8 )];
 __darwin_rune_t __mapupper[(1 <<8 )];






 _RuneRange __runetype_ext;
 _RuneRange __maplower_ext;
 _RuneRange __mapupper_ext;

 void *__variable;
 int __variable_len;




 int __ncharclasses;
 _RuneCharClass *__charclasses;
} _RuneLocale;




extern _RuneLocale _DefaultRuneLocale;
extern _RuneLocale *_CurrentRuneLocale;

# 70 "/usr/include/ctype.h" 2 3 4
# 145 "/usr/include/ctype.h" 3 4

unsigned long ___runetype(__darwin_ct_rune_t);
__darwin_ct_rune_t ___tolower(__darwin_ct_rune_t);
__darwin_ct_rune_t ___toupper(__darwin_ct_rune_t);


static __inline int
isascii(int _c)
{
 return ((_c & ~0x7F) == 0);
}
# 164 "/usr/include/ctype.h" 3 4

int __maskrune(__darwin_ct_rune_t, unsigned long);



static __inline int
__istype(__darwin_ct_rune_t _c, unsigned long _f)
{



 return (isascii(_c) ? !!(_DefaultRuneLocale.__runetype[_c] & _f)
  : !!__maskrune(_c, _f));

}

static __inline __darwin_ct_rune_t
__isctype(__darwin_ct_rune_t _c, unsigned long _f)
{



 return (_c < 0 || _c >= (1 <<8 )) ? 0 :
  !!(_DefaultRuneLocale.__runetype[_c] & _f);

}
# 204 "/usr/include/ctype.h" 3 4

__darwin_ct_rune_t __toupper(__darwin_ct_rune_t);
__darwin_ct_rune_t __tolower(__darwin_ct_rune_t);



static __inline int
__wcwidth(__darwin_ct_rune_t _c)
{
 unsigned int _x;

 if (_c == 0)
  return (0);
 _x = (unsigned int)__maskrune(_c, 0xe0000000L|0x00040000L);
 if ((_x & 0xe0000000L) != 0)
  return ((_x & 0xe0000000L) >> 30);
 return ((_x & 0x00040000L) != 0 ? 1 : -1);
}






static __inline int
isalnum(int _c)
{
 return (__istype(_c, 0x00000100L|0x00000400L));
}

static __inline int
isalpha(int _c)
{
 return (__istype(_c, 0x00000100L));
}

static __inline int
isblank(int _c)
{
 return (__istype(_c, 0x00020000L));
}

static __inline int
iscntrl(int _c)
{
 return (__istype(_c, 0x00000200L));
}


static __inline int
isdigit(int _c)
{
 return (__isctype(_c, 0x00000400L));
}

static __inline int
isgraph(int _c)
{
 return (__istype(_c, 0x00000800L));
}

static __inline int
islower(int _c)
{
 return (__istype(_c, 0x00001000L));
}

static __inline int
isprint(int _c)
{
 return (__istype(_c, 0x00040000L));
}

static __inline int
ispunct(int _c)
{
 return (__istype(_c, 0x00002000L));
}

static __inline int
isspace(int _c)
{
 return (__istype(_c, 0x00004000L));
}

static __inline int
isupper(int _c)
{
 return (__istype(_c, 0x00008000L));
}


static __inline int
isxdigit(int _c)
{
 return (__isctype(_c, 0x00010000L));
}

static __inline int
toascii(int _c)
{
 return (_c & 0x7F);
}

static __inline int
tolower(int _c)
{
        return (__tolower(_c));
}

static __inline int
toupper(int _c)
{
        return (__toupper(_c));
}


static __inline int
digittoint(int _c)
{
 return (__maskrune(_c, 0x0F));
}

static __inline int
ishexnumber(int _c)
{
 return (__istype(_c, 0x00010000L));
}

static __inline int
isideogram(int _c)
{
 return (__istype(_c, 0x00080000L));
}

static __inline int
isnumber(int _c)
{
 return (__istype(_c, 0x00000400L));
}

static __inline int
isphonogram(int _c)
{
 return (__istype(_c, 0x00200000L));
}

static __inline int
isrune(int _c)
{
 return (__istype(_c, 0xFFFFFFF0L));
}

static __inline int
isspecial(int _c)
{
 return (__istype(_c, 0x00100000L));
}
# 73 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2


# 1 "/usr/include/math.h" 1 3 4
# 28 "/usr/include/math.h" 3 4
# 1 "/usr/include/architecture/i386/math.h" 1 3 4
# 49 "/usr/include/architecture/i386/math.h" 3 4
 typedef float float_t;
 typedef double double_t;
# 108 "/usr/include/architecture/i386/math.h" 3 4
extern int __math_errhandling ( void );
# 128 "/usr/include/architecture/i386/math.h" 3 4
extern int __fpclassifyf(float );
extern int __fpclassifyd(double );
extern int __fpclassify (long double);
# 163 "/usr/include/architecture/i386/math.h" 3 4
 static __inline__ int __inline_isfinitef (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinited (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isfinite (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isinff (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinfd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isinf (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnanf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnand (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnan (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormalf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormald (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_isnormal (long double) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitf (float ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbitd (double ) __attribute__ ((always_inline));
 static __inline__ int __inline_signbit (long double) __attribute__ ((always_inline));

 static __inline__ int __inline_isinff( float __x ) { return __builtin_fabsf(__x) == __builtin_inff(); }
 static __inline__ int __inline_isinfd( double __x ) { return __builtin_fabs(__x) == __builtin_inf(); }
 static __inline__ int __inline_isinf( long double __x ) { return __builtin_fabsl(__x) == __builtin_infl(); }
 static __inline__ int __inline_isfinitef( float __x ) { return __x == __x && __builtin_fabsf(__x) != __builtin_inff(); }
 static __inline__ int __inline_isfinited( double __x ) { return __x == __x && __builtin_fabs(__x) != __builtin_inf(); }
 static __inline__ int __inline_isfinite( long double __x ) { return __x == __x && __builtin_fabsl(__x) != __builtin_infl(); }
 static __inline__ int __inline_isnanf( float __x ) { return __x != __x; }
 static __inline__ int __inline_isnand( double __x ) { return __x != __x; }
 static __inline__ int __inline_isnan( long double __x ) { return __x != __x; }
 static __inline__ int __inline_signbitf( float __x ) { union{ float __f; unsigned int __u; }__u; __u.__f = __x; return (int)(__u.__u >> 31); }
 static __inline__ int __inline_signbitd( double __x ) { union{ double __f; unsigned int __u[2]; }__u; __u.__f = __x; return (int)(__u.__u[1] >> 31); }
 static __inline__ int __inline_signbit( long double __x ){ union{ long double __ld; struct{ unsigned int __m[2]; short __sexp; }__p; }__u; __u.__ld = __x; return (int) (((unsigned short) __u.__p.__sexp) >> 15); }
 static __inline__ int __inline_isnormalf( float __x ) { float fabsf = __builtin_fabsf(__x); if( __x != __x ) return 0; return fabsf < __builtin_inff() && fabsf >= 1.17549435e-38F; }
 static __inline__ int __inline_isnormald( double __x ) { double fabsf = __builtin_fabs(__x); if( __x != __x ) return 0; return fabsf < __builtin_inf() && fabsf >= 2.2250738585072014e-308; }
 static __inline__ int __inline_isnormal( long double __x ) { long double fabsf = __builtin_fabsl(__x); if( __x != __x ) return 0; return fabsf < __builtin_infl() && fabsf >= 3.36210314311209350626e-4932L; }
# 253 "/usr/include/architecture/i386/math.h" 3 4
extern double acos( double );
extern float acosf( float );

extern double asin( double );
extern float asinf( float );

extern double atan( double );
extern float atanf( float );

extern double atan2( double, double );
extern float atan2f( float, float );

extern double cos( double );
extern float cosf( float );

extern double sin( double );
extern float sinf( float );

extern double tan( double );
extern float tanf( float );

extern double acosh( double );
extern float acoshf( float );

extern double asinh( double );
extern float asinhf( float );

extern double atanh( double );
extern float atanhf( float );

extern double cosh( double );
extern float coshf( float );

extern double sinh( double );
extern float sinhf( float );

extern double tanh( double );
extern float tanhf( float );

extern double exp ( double );
extern float expf ( float );

extern double exp2 ( double );
extern float exp2f ( float );

extern double expm1 ( double );
extern float expm1f ( float );

extern double log ( double );
extern float logf ( float );

extern double log10 ( double );
extern float log10f ( float );

extern double log2 ( double );
extern float log2f ( float );

extern double log1p ( double );
extern float log1pf ( float );

extern double logb ( double );
extern float logbf ( float );

extern double modf ( double, double * );
extern float modff ( float, float * );

extern double ldexp ( double, int );
extern float ldexpf ( float, int );

extern double frexp ( double, int * );
extern float frexpf ( float, int * );

extern int ilogb ( double );
extern int ilogbf ( float );

extern double scalbn ( double, int );
extern float scalbnf ( float, int );

extern double scalbln ( double, long int );
extern float scalblnf ( float, long int );

extern double fabs( double );
extern float fabsf( float );

extern double cbrt( double );
extern float cbrtf( float );

extern double hypot ( double, double );
extern float hypotf ( float, float );

extern double pow ( double, double );
extern float powf ( float, float );

extern double sqrt( double );
extern float sqrtf( float );

extern double erf( double );
extern float erff( float );

extern double erfc( double );
extern float erfcf( float );






extern double lgamma( double );
extern float lgammaf( float );

extern double tgamma( double );
extern float tgammaf( float );

extern double ceil ( double );
extern float ceilf ( float );

extern double floor ( double );
extern float floorf ( float );

extern double nearbyint ( double );
extern float nearbyintf ( float );

extern double rint ( double );
extern float rintf ( float );

extern long int lrint ( double );
extern long int lrintf ( float );

extern double round ( double );
extern float roundf ( float );

extern long int lround ( double );
extern long int lroundf ( float );



    extern long long int llrint ( double );
    extern long long int llrintf ( float );
    extern long long int llround ( double );
    extern long long int llroundf ( float );


extern double trunc ( double );
extern float truncf ( float );

extern double fmod ( double, double );
extern float fmodf ( float, float );

extern double remainder ( double, double );
extern float remainderf ( float, float );

extern double remquo ( double, double, int * );
extern float remquof ( float, float, int * );

extern double copysign ( double, double );
extern float copysignf ( float, float );

extern double nan( const char * );
extern float nanf( const char * );

extern double nextafter ( double, double );
extern float nextafterf ( float, float );

extern double fdim ( double, double );
extern float fdimf ( float, float );

extern double fmax ( double, double );
extern float fmaxf ( float, float );

extern double fmin ( double, double );
extern float fminf ( float, float );

extern double fma ( double, double, double );
extern float fmaf ( float, float, float );

extern long double acosl(long double);
extern long double asinl(long double);
extern long double atanl(long double);
extern long double atan2l(long double, long double);
extern long double cosl(long double);
extern long double sinl(long double);
extern long double tanl(long double);
extern long double acoshl(long double);
extern long double asinhl(long double);
extern long double atanhl(long double);
extern long double coshl(long double);
extern long double sinhl(long double);
extern long double tanhl(long double);
extern long double expl(long double);
extern long double exp2l(long double);
extern long double expm1l(long double);
extern long double logl(long double);
extern long double log10l(long double);
extern long double log2l(long double);
extern long double log1pl(long double);
extern long double logbl(long double);
extern long double modfl(long double, long double *);
extern long double ldexpl(long double, int);
extern long double frexpl(long double, int *);
extern int ilogbl(long double);
extern long double scalbnl(long double, int);
extern long double scalblnl(long double, long int);
extern long double fabsl(long double);
extern long double cbrtl(long double);
extern long double hypotl(long double, long double);
extern long double powl(long double, long double);
extern long double sqrtl(long double);
extern long double erfl(long double);
extern long double erfcl(long double);






extern long double lgammal(long double);

extern long double tgammal(long double);
extern long double ceill(long double);
extern long double floorl(long double);
extern long double nearbyintl(long double);
extern long double rintl(long double);
extern long int lrintl(long double);
extern long double roundl(long double);
extern long int lroundl(long double);



    extern long long int llrintl(long double);
    extern long long int llroundl(long double);


extern long double truncl(long double);
extern long double fmodl(long double, long double);
extern long double remainderl(long double, long double);
extern long double remquol(long double, long double, int *);
extern long double copysignl(long double, long double);
extern long double nanl(const char *);
extern long double nextafterl(long double, long double);
extern double nexttoward(double, long double);
extern float nexttowardf(float, long double);
extern long double nexttowardl(long double, long double);
extern long double fdiml(long double, long double);
extern long double fmaxl(long double, long double);
extern long double fminl(long double, long double);
extern long double fmal(long double, long double, long double);
# 507 "/usr/include/architecture/i386/math.h" 3 4
extern double __inf( void );
extern float __inff( void );
extern long double __infl( void );
extern float __nan( void );


extern double j0 ( double );

extern double j1 ( double );

extern double jn ( int, double );

extern double y0 ( double );

extern double y1 ( double );

extern double yn ( int, double );

extern double scalb ( double, double );
# 543 "/usr/include/architecture/i386/math.h" 3 4
extern int signgam;
# 558 "/usr/include/architecture/i386/math.h" 3 4
extern long int rinttol ( double );


extern long int roundtol ( double );
# 570 "/usr/include/architecture/i386/math.h" 3 4
struct exception {
 int type;
 char *name;
 double arg1;
 double arg2;
 double retval;
};
# 601 "/usr/include/architecture/i386/math.h" 3 4
extern int finite ( double );


extern double gamma ( double );




extern int matherr ( struct exception * );





extern double significand ( double );






extern double drem ( double, double );
# 29 "/usr/include/math.h" 2 3 4
# 76 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 115 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
typedef enum
{
    SDL_FALSE = 0,
    SDL_TRUE = 1
} SDL_bool;




typedef int8_t Sint8;



typedef uint8_t Uint8;



typedef int16_t Sint16;



typedef uint16_t Uint16;



typedef int32_t Sint32;



typedef uint32_t Uint32;




typedef int64_t Sint64;



typedef uint64_t Uint64;
# 162 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
typedef int SDL_dummy_uint8[(sizeof(Uint8) == 1) * 2 - 1];
typedef int SDL_dummy_sint8[(sizeof(Sint8) == 1) * 2 - 1];
typedef int SDL_dummy_uint16[(sizeof(Uint16) == 2) * 2 - 1];
typedef int SDL_dummy_sint16[(sizeof(Sint16) == 2) * 2 - 1];
typedef int SDL_dummy_uint32[(sizeof(Uint32) == 4) * 2 - 1];
typedef int SDL_dummy_sint32[(sizeof(Sint32) == 4) * 2 - 1];
typedef int SDL_dummy_uint64[(sizeof(Uint64) == 8) * 2 - 1];
typedef int SDL_dummy_sint64[(sizeof(Sint64) == 8) * 2 - 1];
# 187 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
typedef enum
{
    DUMMY_ENUM_VALUE
} SDL_DUMMY_ENUM;

typedef int SDL_dummy_enum[(sizeof(SDL_DUMMY_ENUM) == sizeof(int)) * 2 - 1];




# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 198 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 418 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) size_t SDL_wcslen(const wchar_t * string);





extern __attribute__ ((visibility("default"))) size_t SDL_wcslcpy(wchar_t *dst, const wchar_t *src, size_t maxlen);





extern __attribute__ ((visibility("default"))) size_t SDL_wcslcat(wchar_t *dst, const wchar_t *src, size_t maxlen);
# 441 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) size_t SDL_utf8strlcpy(char *dst, const char *src,
                                            size_t dst_bytes);
# 460 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) char * SDL_strrev(char *string);





extern __attribute__ ((visibility("default"))) char * SDL_strupr(char *string);





extern __attribute__ ((visibility("default"))) char * SDL_strlwr(char *string);
# 507 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) char * SDL_ltoa(long value, char *string, int radix);
# 519 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) char * SDL_ultoa(unsigned long value, char *string,
                                        int radix);
# 540 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
extern __attribute__ ((visibility("default"))) char * SDL_lltoa(Sint64 value, char *string,
                                        int radix);





extern __attribute__ ((visibility("default"))) char * SDL_ulltoa(Uint64 value, char *string,
                                         int radix);
# 734 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
typedef struct _SDL_iconv_t *SDL_iconv_t;
extern __attribute__ ((visibility("default"))) SDL_iconv_t SDL_iconv_open(const char *tocode,
                                                   const char *fromcode);
extern __attribute__ ((visibility("default"))) int SDL_iconv_close(SDL_iconv_t cd);

extern __attribute__ ((visibility("default"))) size_t SDL_iconv(SDL_iconv_t cd, const char **inbuf,
                                         size_t * inbytesleft, char **outbuf,
                                         size_t * outbytesleft);




extern __attribute__ ((visibility("default"))) char * SDL_iconv_string(const char *tocode,
                                               const char *fromcode,
                                               const char *inbuf,
                                               size_t inbytesleft);
# 760 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 761 "../Frameworks/SDL.framework/Headers/SDL_stdinc.h" 2
# 26 "../Frameworks/SDL.framework/Headers/SDL_main.h" 2
# 67 "../Frameworks/SDL.framework/Headers/SDL_main.h"
extern int SDL_main(int argc, char *argv[]);


# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 71 "../Frameworks/SDL.framework/Headers/SDL_main.h" 2
# 94 "../Frameworks/SDL.framework/Headers/SDL_main.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 95 "../Frameworks/SDL.framework/Headers/SDL_main.h" 2
# 73 "../Frameworks/SDL.framework/Headers/SDL.h" 2

# 1 "../Frameworks/SDL.framework/Headers/SDL_assert.h" 1
# 27 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 28 "../Frameworks/SDL.framework/Headers/SDL_assert.h" 2
# 96 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
typedef enum
{
    SDL_ASSERTION_RETRY,
    SDL_ASSERTION_BREAK,
    SDL_ASSERTION_ABORT,
    SDL_ASSERTION_IGNORE,
    SDL_ASSERTION_ALWAYS_IGNORE
} SDL_assert_state;

typedef struct SDL_assert_data
{
    int always_ignore;
    unsigned int trigger_count;
    const char *condition;
    const char *filename;
    int linenum;
    const char *function;
    const struct SDL_assert_data *next;
} SDL_assert_data;


extern __attribute__ ((visibility("default"))) SDL_assert_state SDL_ReportAssertion(SDL_assert_data *,
                                                             const char *,
                                                             const char *, int);
# 171 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
typedef SDL_assert_state ( *SDL_AssertionHandler)(
                                 const SDL_assert_data* data, void* userdata);
# 194 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
extern __attribute__ ((visibility("default"))) void SDL_SetAssertionHandler(
                                            SDL_AssertionHandler handler,
                                            void *userdata);
# 220 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
extern __attribute__ ((visibility("default"))) const SDL_assert_data * SDL_GetAssertionReport(void);
# 229 "../Frameworks/SDL.framework/Headers/SDL_assert.h"
extern __attribute__ ((visibility("default"))) void SDL_ResetAssertionReport(void);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 238 "../Frameworks/SDL.framework/Headers/SDL_assert.h" 2
# 75 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_atomic.h" 1
# 64 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 65 "../Frameworks/SDL.framework/Headers/SDL_atomic.h" 2
# 97 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
typedef int SDL_SpinLock;
# 106 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_AtomicTryLock(SDL_SpinLock *lock);






extern __attribute__ ((visibility("default"))) void SDL_AtomicLock(SDL_SpinLock *lock);






extern __attribute__ ((visibility("default"))) void SDL_AtomicUnlock(SDL_SpinLock *lock);
# 161 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
# 1 "/usr/include/libkern/OSAtomic.h" 1 3 4
# 27 "/usr/include/libkern/OSAtomic.h" 3 4
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stddef.h" 1 3 4
# 28 "/usr/include/libkern/OSAtomic.h" 2 3 4


# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/stdbool.h" 1 3 4
# 31 "/usr/include/libkern/OSAtomic.h" 2 3 4
# 76 "/usr/include/libkern/OSAtomic.h" 3 4

# 90 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAdd32( int32_t __theAmount, volatile int32_t *__theValue );
# 103 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAdd32Barrier( int32_t __theAmount, volatile int32_t *__theValue );




__inline static
int32_t OSAtomicIncrement32( volatile int32_t *__theValue )
            { return OSAtomicAdd32( 1, __theValue); }
# 119 "/usr/include/libkern/OSAtomic.h" 3 4
__inline static
int32_t OSAtomicIncrement32Barrier( volatile int32_t *__theValue )
            { return OSAtomicAdd32Barrier( 1, __theValue); }


__inline static
int32_t OSAtomicDecrement32( volatile int32_t *__theValue )
            { return OSAtomicAdd32( -1, __theValue); }







__inline static
int32_t OSAtomicDecrement32Barrier( volatile int32_t *__theValue )
            { return OSAtomicAdd32Barrier( -1, __theValue); }
# 147 "/usr/include/libkern/OSAtomic.h" 3 4
int64_t OSAtomicAdd64( int64_t __theAmount, volatile int64_t *__theValue );
# 160 "/usr/include/libkern/OSAtomic.h" 3 4
int64_t OSAtomicAdd64Barrier( int64_t __theAmount, volatile int64_t *__theValue ) __attribute__((visibility("default")));



__inline static
int64_t OSAtomicIncrement64( volatile int64_t *__theValue )
            { return OSAtomicAdd64( 1, __theValue); }







__inline static
int64_t OSAtomicIncrement64Barrier( volatile int64_t *__theValue )
            { return OSAtomicAdd64Barrier( 1, __theValue); }
# 185 "/usr/include/libkern/OSAtomic.h" 3 4
__inline static
int64_t OSAtomicDecrement64( volatile int64_t *__theValue )
            { return OSAtomicAdd64( -1, __theValue); }
# 196 "/usr/include/libkern/OSAtomic.h" 3 4
__inline static
int64_t OSAtomicDecrement64Barrier( volatile int64_t *__theValue )
            { return OSAtomicAdd64Barrier( -1, __theValue); }
# 222 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicOr32( uint32_t __theMask, volatile uint32_t *__theValue );
# 235 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicOr32Barrier( uint32_t __theMask, volatile uint32_t *__theValue );
# 245 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicOr32Orig( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 258 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicOr32OrigBarrier( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 270 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAnd32( uint32_t __theMask, volatile uint32_t *__theValue );
# 283 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAnd32Barrier( uint32_t __theMask, volatile uint32_t *__theValue );
# 293 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAnd32Orig( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 306 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicAnd32OrigBarrier( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 318 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicXor32( uint32_t __theMask, volatile uint32_t *__theValue );
# 331 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicXor32Barrier( uint32_t __theMask, volatile uint32_t *__theValue );
# 341 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicXor32Orig( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 354 "/usr/include/libkern/OSAtomic.h" 3 4
int32_t OSAtomicXor32OrigBarrier( uint32_t __theMask, volatile uint32_t *__theValue ) __attribute__((visibility("default")));
# 371 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwap32( int32_t __oldValue, int32_t __newValue, volatile int32_t *__theValue );
# 385 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwap32Barrier( int32_t __oldValue, int32_t __newValue, volatile int32_t *__theValue );
# 396 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapPtr( void *__oldValue, void *__newValue, void * volatile *__theValue ) __attribute__((visibility("default")));
# 410 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapPtrBarrier( void *__oldValue, void *__newValue, void * volatile *__theValue ) __attribute__((visibility("default")));
# 423 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapInt( int __oldValue, int __newValue, volatile int *__theValue ) __attribute__((visibility("default")));
# 439 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapIntBarrier( int __oldValue, int __newValue, volatile int *__theValue ) __attribute__((visibility("default")));
# 453 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapLong( long __oldValue, long __newValue, volatile long *__theValue ) __attribute__((visibility("default")));
# 470 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwapLongBarrier( long __oldValue, long __newValue, volatile long *__theValue ) __attribute__((visibility("default")));
# 483 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwap64( int64_t __oldValue, int64_t __newValue, volatile int64_t *__theValue );
# 497 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicCompareAndSwap64Barrier( int64_t __oldValue, int64_t __newValue, volatile int64_t *__theValue ) __attribute__((visibility("default")));
# 517 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicTestAndSet( uint32_t __n, volatile void *__theAddress );
# 536 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicTestAndSetBarrier( uint32_t __n, volatile void *__theAddress );
# 552 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicTestAndClear( uint32_t __n, volatile void *__theAddress );
# 570 "/usr/include/libkern/OSAtomic.h" 3 4
_Bool OSAtomicTestAndClearBarrier( uint32_t __n, volatile void *__theAddress );
# 590 "/usr/include/libkern/OSAtomic.h" 3 4
typedef int32_t OSSpinLock;







_Bool OSSpinLockTry( volatile OSSpinLock *__lock );
# 607 "/usr/include/libkern/OSAtomic.h" 3 4
void OSSpinLockLock( volatile OSSpinLock *__lock );



void OSSpinLockUnlock( volatile OSSpinLock *__lock );
# 625 "/usr/include/libkern/OSAtomic.h" 3 4
typedef volatile struct {
 void *opaque1;
 long opaque2;
} __attribute__ ((aligned (16))) OSQueueHead;
# 660 "/usr/include/libkern/OSAtomic.h" 3 4
void OSAtomicEnqueue( OSQueueHead *__list, void *__new, size_t __offset) __attribute__((visibility("default")));
# 682 "/usr/include/libkern/OSAtomic.h" 3 4
void* OSAtomicDequeue( OSQueueHead *__list, size_t __offset) __attribute__((visibility("default")));
# 697 "/usr/include/libkern/OSAtomic.h" 3 4
typedef volatile struct {
 void *opaque1;
 void *opaque2;
 int opaque3;
} __attribute__ ((aligned (16))) OSFifoQueueHead;
# 734 "/usr/include/libkern/OSAtomic.h" 3 4
void OSAtomicFifoEnqueue( OSFifoQueueHead *__list, void *__new, size_t __offset) __attribute__((visibility("default")));
# 755 "/usr/include/libkern/OSAtomic.h" 3 4
void* OSAtomicFifoDequeue( OSFifoQueueHead *__list, size_t __offset) __attribute__((visibility("default")));
# 765 "/usr/include/libkern/OSAtomic.h" 3 4
void OSMemoryBarrier( void );


# 162 "../Frameworks/SDL.framework/Headers/SDL_atomic.h" 2
# 188 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
typedef struct { int value; } SDL_atomic_t;
# 201 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_AtomicCAS_(SDL_atomic_t *a, int oldval, int newval);







static __inline__ int SDL_AtomicSet(SDL_atomic_t *a, int v)
{
    int value;
    do {
        value = a->value;
    } while (!OSAtomicCompareAndSwap32Barrier((value), (v), &(a)->value));
    return value;
}






static __inline__ int SDL_AtomicGet(SDL_atomic_t *a)
{
    int value = a->value;
    __asm__ __volatile__ ("" : : : "memory");
    return value;
}
# 239 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
static __inline__ int SDL_AtomicAdd(SDL_atomic_t *a, int v)
{
    int value;
    do {
        value = a->value;
    } while (!OSAtomicCompareAndSwap32Barrier((value), ((value + v)), &(a)->value));
    return value;
}
# 276 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_AtomicCASPtr_(void* *a, void *oldval, void *newval);







static __inline__ void* SDL_AtomicSetPtr(void* *a, void* v)
{
    void* value;
    do {
        value = *a;
    } while (!OSAtomicCompareAndSwap64Barrier((int64_t)(value), (int64_t)(v), (int64_t*)(a)));
    return value;
}






static __inline__ void* SDL_AtomicGetPtr(void* *a)
{
    void* value = *a;
    __asm__ __volatile__ ("" : : : "memory");
    return value;
}
# 314 "../Frameworks/SDL.framework/Headers/SDL_atomic.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 315 "../Frameworks/SDL.framework/Headers/SDL_atomic.h" 2
# 76 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 1
# 32 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_error.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_error.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_error.h" 2
# 42 "../Frameworks/SDL.framework/Headers/SDL_error.h"
extern __attribute__ ((visibility("default"))) void SDL_SetError(const char *fmt, ...);
extern __attribute__ ((visibility("default"))) const char * SDL_GetError(void);
extern __attribute__ ((visibility("default"))) void SDL_ClearError(void);
# 55 "../Frameworks/SDL.framework/Headers/SDL_error.h"
typedef enum
{
    SDL_ENOMEM,
    SDL_EFREAD,
    SDL_EFWRITE,
    SDL_EFSEEK,
    SDL_UNSUPPORTED,
    SDL_LASTERROR
} SDL_errorcode;
extern __attribute__ ((visibility("default"))) void SDL_Error(SDL_errorcode code);
# 73 "../Frameworks/SDL.framework/Headers/SDL_error.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 74 "../Frameworks/SDL.framework/Headers/SDL_error.h" 2
# 33 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_endian.h" 1
# 59 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 60 "../Frameworks/SDL.framework/Headers/SDL_endian.h" 2
# 84 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
static __inline__ Uint16
SDL_Swap16(Uint16 x)
{
  __asm__("xchgb %b0,%h0": "=Q"(x):"0"(x));
    return x;
}
# 122 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
static __inline__ Uint32
SDL_Swap32(Uint32 x)
{
  __asm__("bswapl %0": "=r"(x):"0"(x));
    return x;
}
# 174 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
static __inline__ Uint64
SDL_Swap64(Uint64 x)
{
  __asm__("bswapq %0": "=r"(x):"0"(x));
    return x;
}
# 198 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
static __inline__ float
SDL_SwapFloat(float x)
{
    union
    {
        float f;
        Uint32 ui32;
    } swapper;
    swapper.f = x;
    swapper.ui32 = SDL_Swap32(swapper.ui32);
    return swapper.f;
}
# 244 "../Frameworks/SDL.framework/Headers/SDL_endian.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 245 "../Frameworks/SDL.framework/Headers/SDL_endian.h" 2
# 34 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_mutex.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_mutex.h" 2
# 60 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
struct SDL_mutex;
typedef struct SDL_mutex SDL_mutex;




extern __attribute__ ((visibility("default"))) SDL_mutex * SDL_CreateMutex(void);







extern __attribute__ ((visibility("default"))) int SDL_mutexP(SDL_mutex * mutex);
# 85 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
extern __attribute__ ((visibility("default"))) int SDL_mutexV(SDL_mutex * mutex);




extern __attribute__ ((visibility("default"))) void SDL_DestroyMutex(SDL_mutex * mutex);
# 101 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
struct SDL_semaphore;
typedef struct SDL_semaphore SDL_sem;




extern __attribute__ ((visibility("default"))) SDL_sem * SDL_CreateSemaphore(Uint32 initial_value);




extern __attribute__ ((visibility("default"))) void SDL_DestroySemaphore(SDL_sem * sem);






extern __attribute__ ((visibility("default"))) int SDL_SemWait(SDL_sem * sem);







extern __attribute__ ((visibility("default"))) int SDL_SemTryWait(SDL_sem * sem);
# 138 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
extern __attribute__ ((visibility("default"))) int SDL_SemWaitTimeout(SDL_sem * sem, Uint32 ms);






extern __attribute__ ((visibility("default"))) int SDL_SemPost(SDL_sem * sem);




extern __attribute__ ((visibility("default"))) Uint32 SDL_SemValue(SDL_sem * sem);
# 161 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
struct SDL_cond;
typedef struct SDL_cond SDL_cond;
# 192 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
extern __attribute__ ((visibility("default"))) SDL_cond * SDL_CreateCond(void);




extern __attribute__ ((visibility("default"))) void SDL_DestroyCond(SDL_cond * cond);






extern __attribute__ ((visibility("default"))) int SDL_CondSignal(SDL_cond * cond);






extern __attribute__ ((visibility("default"))) int SDL_CondBroadcast(SDL_cond * cond);
# 222 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
extern __attribute__ ((visibility("default"))) int SDL_CondWait(SDL_cond * cond, SDL_mutex * mutex);
# 232 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
extern __attribute__ ((visibility("default"))) int SDL_CondWaitTimeout(SDL_cond * cond,
                                                SDL_mutex * mutex, Uint32 ms);
# 244 "../Frameworks/SDL.framework/Headers/SDL_mutex.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 245 "../Frameworks/SDL.framework/Headers/SDL_mutex.h" 2
# 35 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_thread.h" 1
# 37 "../Frameworks/SDL.framework/Headers/SDL_thread.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 38 "../Frameworks/SDL.framework/Headers/SDL_thread.h" 2
# 46 "../Frameworks/SDL.framework/Headers/SDL_thread.h"
struct SDL_Thread;
typedef struct SDL_Thread SDL_Thread;


typedef unsigned long SDL_threadID;





typedef enum {
    SDL_THREAD_PRIORITY_LOW,
    SDL_THREAD_PRIORITY_NORMAL,
    SDL_THREAD_PRIORITY_HIGH
} SDL_ThreadPriority;




typedef int ( * SDL_ThreadFunction) (void *data);
# 144 "../Frameworks/SDL.framework/Headers/SDL_thread.h"
extern __attribute__ ((visibility("default"))) SDL_Thread *
SDL_CreateThread(SDL_ThreadFunction fn, const char *name, void *data);
# 156 "../Frameworks/SDL.framework/Headers/SDL_thread.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetThreadName(SDL_Thread *thread);




extern __attribute__ ((visibility("default"))) SDL_threadID SDL_ThreadID(void);






extern __attribute__ ((visibility("default"))) SDL_threadID SDL_GetThreadID(SDL_Thread * thread);




extern __attribute__ ((visibility("default"))) int SDL_SetThreadPriority(SDL_ThreadPriority priority);







extern __attribute__ ((visibility("default"))) void SDL_WaitThread(SDL_Thread * thread, int *status);
# 190 "../Frameworks/SDL.framework/Headers/SDL_thread.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 191 "../Frameworks/SDL.framework/Headers/SDL_thread.h" 2
# 36 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_rwops.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 36 "../Frameworks/SDL.framework/Headers/SDL_rwops.h" 2
# 46 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
typedef struct SDL_RWops
{






    long ( * seek) (struct SDL_RWops * context, long offset,
                           int whence);







    size_t( * read) (struct SDL_RWops * context, void *ptr,
                            size_t size, size_t maxnum);







    size_t( * write) (struct SDL_RWops * context, const void *ptr,
                             size_t size, size_t num);






    int ( * close) (struct SDL_RWops * context);

    Uint32 type;
    union
    {
# 113 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
        struct
        {
            SDL_bool autoclose;
            FILE *fp;
        } stdio;

        struct
        {
            Uint8 *base;
            Uint8 *here;
            Uint8 *stop;
        } mem;
        struct
        {
            void *data1;
        } unknown;
    } hidden;

} SDL_RWops;
# 141 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
extern __attribute__ ((visibility("default"))) SDL_RWops * SDL_RWFromFile(const char *file,
                                                  const char *mode);


extern __attribute__ ((visibility("default"))) SDL_RWops * SDL_RWFromFP(FILE * fp,
                                                SDL_bool autoclose);





extern __attribute__ ((visibility("default"))) SDL_RWops * SDL_RWFromMem(void *mem, int size);
extern __attribute__ ((visibility("default"))) SDL_RWops * SDL_RWFromConstMem(const void *mem,
                                                      int size);




extern __attribute__ ((visibility("default"))) SDL_RWops * SDL_AllocRW(void);
extern __attribute__ ((visibility("default"))) void SDL_FreeRW(SDL_RWops * area);
# 186 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
extern __attribute__ ((visibility("default"))) Uint16 SDL_ReadLE16(SDL_RWops * src);
extern __attribute__ ((visibility("default"))) Uint16 SDL_ReadBE16(SDL_RWops * src);
extern __attribute__ ((visibility("default"))) Uint32 SDL_ReadLE32(SDL_RWops * src);
extern __attribute__ ((visibility("default"))) Uint32 SDL_ReadBE32(SDL_RWops * src);
extern __attribute__ ((visibility("default"))) Uint64 SDL_ReadLE64(SDL_RWops * src);
extern __attribute__ ((visibility("default"))) Uint64 SDL_ReadBE64(SDL_RWops * src);
# 200 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
extern __attribute__ ((visibility("default"))) size_t SDL_WriteLE16(SDL_RWops * dst, Uint16 value);
extern __attribute__ ((visibility("default"))) size_t SDL_WriteBE16(SDL_RWops * dst, Uint16 value);
extern __attribute__ ((visibility("default"))) size_t SDL_WriteLE32(SDL_RWops * dst, Uint32 value);
extern __attribute__ ((visibility("default"))) size_t SDL_WriteBE32(SDL_RWops * dst, Uint32 value);
extern __attribute__ ((visibility("default"))) size_t SDL_WriteLE64(SDL_RWops * dst, Uint64 value);
extern __attribute__ ((visibility("default"))) size_t SDL_WriteBE64(SDL_RWops * dst, Uint64 value);
# 215 "../Frameworks/SDL.framework/Headers/SDL_rwops.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 216 "../Frameworks/SDL.framework/Headers/SDL_rwops.h" 2
# 37 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2

# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 39 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 66 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
typedef Uint16 SDL_AudioFormat;
# 165 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
typedef void ( * SDL_AudioCallback) (void *userdata, Uint8 * stream,
                                            int len);




typedef struct SDL_AudioSpec
{
    int freq;
    SDL_AudioFormat format;
    Uint8 channels;
    Uint8 silence;
    Uint16 samples;
    Uint16 padding;
    Uint32 size;
    SDL_AudioCallback callback;
    void *userdata;
} SDL_AudioSpec;


struct SDL_AudioCVT;
typedef void ( * SDL_AudioFilter) (struct SDL_AudioCVT * cvt,
                                          SDL_AudioFormat format);




typedef struct SDL_AudioCVT
{
    int needed;
    SDL_AudioFormat src_format;
    SDL_AudioFormat dst_format;
    double rate_incr;
    Uint8 *buf;
    int len;
    int len_cvt;
    int len_mult;
    double len_ratio;
    SDL_AudioFilter filters[10];
    int filter_index;
} SDL_AudioCVT;
# 217 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_GetNumAudioDrivers(void);
extern __attribute__ ((visibility("default"))) const char * SDL_GetAudioDriver(int index);
# 229 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_AudioInit(const char *driver_name);
extern __attribute__ ((visibility("default"))) void SDL_AudioQuit(void);






extern __attribute__ ((visibility("default"))) const char * SDL_GetCurrentAudioDriver(void);
# 281 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_OpenAudio(SDL_AudioSpec * desired,
                                          SDL_AudioSpec * obtained);
# 293 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
typedef Uint32 SDL_AudioDeviceID;
# 307 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_GetNumAudioDevices(int iscapture);
# 322 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetAudioDeviceName(int index,
                                                           int iscapture);
# 339 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) SDL_AudioDeviceID SDL_OpenAudioDevice(const char
                                                              *device,
                                                              int iscapture,
                                                              const
                                                              SDL_AudioSpec *
                                                              desired,
                                                              SDL_AudioSpec *
                                                              obtained,
                                                              int
                                                              allowed_changes);
# 358 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
typedef enum
{
    SDL_AUDIO_STOPPED = 0,
    SDL_AUDIO_PLAYING,
    SDL_AUDIO_PAUSED
} SDL_AudioStatus;
extern __attribute__ ((visibility("default"))) SDL_AudioStatus SDL_GetAudioStatus(void);

extern __attribute__ ((visibility("default"))) SDL_AudioStatus
SDL_GetAudioDeviceStatus(SDL_AudioDeviceID dev);
# 380 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) void SDL_PauseAudio(int pause_on);
extern __attribute__ ((visibility("default"))) void SDL_PauseAudioDevice(SDL_AudioDeviceID dev,
                                                  int pause_on);
# 404 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) SDL_AudioSpec * SDL_LoadWAV_RW(SDL_RWops * src,
                                                      int freesrc,
                                                      SDL_AudioSpec * spec,
                                                      Uint8 ** audio_buf,
                                                      Uint32 * audio_len);
# 420 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) void SDL_FreeWAV(Uint8 * audio_buf);
# 431 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_BuildAudioCVT(SDL_AudioCVT * cvt,
                                              SDL_AudioFormat src_format,
                                              Uint8 src_channels,
                                              int src_rate,
                                              SDL_AudioFormat dst_format,
                                              Uint8 dst_channels,
                                              int dst_rate);
# 449 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) int SDL_ConvertAudio(SDL_AudioCVT * cvt);
# 459 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) void SDL_MixAudio(Uint8 * dst, const Uint8 * src,
                                          Uint32 len, int volume);






extern __attribute__ ((visibility("default"))) void SDL_MixAudioFormat(Uint8 * dst,
                                                const Uint8 * src,
                                                SDL_AudioFormat format,
                                                Uint32 len, int volume);
# 481 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
extern __attribute__ ((visibility("default"))) void SDL_LockAudio(void);
extern __attribute__ ((visibility("default"))) void SDL_LockAudioDevice(SDL_AudioDeviceID dev);
extern __attribute__ ((visibility("default"))) void SDL_UnlockAudio(void);
extern __attribute__ ((visibility("default"))) void SDL_UnlockAudioDevice(SDL_AudioDeviceID dev);





extern __attribute__ ((visibility("default"))) void SDL_CloseAudio(void);
extern __attribute__ ((visibility("default"))) void SDL_CloseAudioDevice(SDL_AudioDeviceID dev);




extern __attribute__ ((visibility("default"))) int SDL_AudioDeviceConnected(SDL_AudioDeviceID dev);
# 505 "../Frameworks/SDL.framework/Headers/SDL_audio.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 506 "../Frameworks/SDL.framework/Headers/SDL_audio.h" 2
# 77 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h" 2
# 48 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h"
extern __attribute__ ((visibility("default"))) int SDL_SetClipboardText(const char *text);






extern __attribute__ ((visibility("default"))) char * SDL_GetClipboardText(void);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasClipboardText(void);
# 71 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 72 "../Frameworks/SDL.framework/Headers/SDL_clipboard.h" 2
# 78 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h" 1
# 53 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h"
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mmintrin.h" 1 3 4
# 41 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mmintrin.h" 3 4
typedef long long __m64 __attribute__ ((__vector_size__ (8), __may_alias__));


typedef int __v2si __attribute__ ((__vector_size__ (8)));
typedef short __v4hi __attribute__ ((__vector_size__ (8)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

typedef long long __v1di __attribute__ ((__vector_size__ (8)));
# 66 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mmintrin.h" 3 4
static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_empty (void)
{
  __builtin_ia32_emms ();
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_m_empty (void)
{
  _mm_empty ();
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi32_si64 (int __i)
{
  return (__m64) __builtin_ia32_vec_init_v2si (__i, 0);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_from_int (int __i)
{
  return _mm_cvtsi32_si64 (__i);
}






static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_from_int64 (long long __i)
{
  return (__m64) __i;
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_m64 (long long __i)
{
  return (__m64) __i;
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64x_si64 (long long __i)
{
  return (__m64) __i;
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set_pi64x (long long __i)
{
  return (__m64) __i;
}




static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_si32 (__m64 __i)
{
  return __builtin_ia32_vec_ext_v2si ((__v2si)__i, 0);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_m_to_int (__m64 __i)
{
  return _mm_cvtsi64_si32 (__i);
}






static __inline long long __attribute__((__always_inline__, __nodebug__))

_m_to_int64 (__m64 __i)
{
  return (long long)__i;
}


static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtm64_si64 (__m64 __i)
{
  return (long long)__i;
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_si64x (__m64 __i)
{
  return (long long)__i;
}






static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_packs_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_packsswb ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_packsswb (__m64 __m1, __m64 __m2)
{
  return _mm_packs_pi16 (__m1, __m2);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_packs_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_packssdw ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_packssdw (__m64 __m1, __m64 __m2)
{
  return _mm_packs_pi32 (__m1, __m2);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_packs_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_packuswb ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_packuswb (__m64 __m1, __m64 __m2)
{
  return _mm_packs_pu16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpckhbw ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpckhbw (__m64 __m1, __m64 __m2)
{
  return _mm_unpackhi_pi8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpckhwd ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpckhwd (__m64 __m1, __m64 __m2)
{
  return _mm_unpackhi_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpckhdq ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpckhdq (__m64 __m1, __m64 __m2)
{
  return _mm_unpackhi_pi32 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpcklbw ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpcklbw (__m64 __m1, __m64 __m2)
{
  return _mm_unpacklo_pi8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpcklwd ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpcklwd (__m64 __m1, __m64 __m2)
{
  return _mm_unpacklo_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_punpckldq ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_punpckldq (__m64 __m1, __m64 __m2)
{
  return _mm_unpacklo_pi32 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_add_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddb (__m64 __m1, __m64 __m2)
{
  return _mm_add_pi8 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_add_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddw (__m64 __m1, __m64 __m2)
{
  return _mm_add_pi16 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_add_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddd ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddd (__m64 __m1, __m64 __m2)
{
  return _mm_add_pi32 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_add_si64 (__m64 __m1, __m64 __m2)
{

  return (__m64) __builtin_ia32_paddq (__m1, __m2);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_adds_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddsb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddsb (__m64 __m1, __m64 __m2)
{
  return _mm_adds_pi8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_adds_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddsw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddsw (__m64 __m1, __m64 __m2)
{
  return _mm_adds_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_adds_pu8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddusb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddusb (__m64 __m1, __m64 __m2)
{
  return _mm_adds_pu8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_adds_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_paddusw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_paddusw (__m64 __m1, __m64 __m2)
{
  return _mm_adds_pu16 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sub_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubb (__m64 __m1, __m64 __m2)
{
  return _mm_sub_pi8 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sub_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubw (__m64 __m1, __m64 __m2)
{
  return _mm_sub_pi16 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sub_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubd ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubd (__m64 __m1, __m64 __m2)
{
  return _mm_sub_pi32 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sub_si64 (__m64 __m1, __m64 __m2)
{

  return (__m64) __builtin_ia32_psubq (__m1, __m2);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_subs_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubsb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubsb (__m64 __m1, __m64 __m2)
{
  return _mm_subs_pi8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_subs_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubsw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubsw (__m64 __m1, __m64 __m2)
{
  return _mm_subs_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_subs_pu8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubusb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubusb (__m64 __m1, __m64 __m2)
{
  return _mm_subs_pu8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_subs_pu16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_psubusw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psubusw (__m64 __m1, __m64 __m2)
{
  return _mm_subs_pu16 (__m1, __m2);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_madd_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pmaddwd ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmaddwd (__m64 __m1, __m64 __m2)
{
  return _mm_madd_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_mulhi_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pmulhw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmulhw (__m64 __m1, __m64 __m2)
{
  return _mm_mulhi_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_mullo_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pmullw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmullw (__m64 __m1, __m64 __m2)
{
  return _mm_mullo_pi16 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sll_pi16 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psllw ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psllw (__m64 __m, __m64 __count)
{
  return _mm_sll_pi16 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_slli_pi16 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psllwi ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psllwi (__m64 __m, int __count)
{
  return _mm_slli_pi16 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sll_pi32 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_pslld ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pslld (__m64 __m, __m64 __count)
{
  return _mm_sll_pi32 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_slli_pi32 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_pslldi ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pslldi (__m64 __m, int __count)
{
  return _mm_slli_pi32 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sll_si64 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psllq (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psllq (__m64 __m, __m64 __count)
{
  return _mm_sll_si64 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_slli_si64 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psllqi (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psllqi (__m64 __m, int __count)
{
  return _mm_slli_si64 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sra_pi16 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psraw ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psraw (__m64 __m, __m64 __count)
{
  return _mm_sra_pi16 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srai_pi16 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psrawi ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrawi (__m64 __m, int __count)
{
  return _mm_srai_pi16 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sra_pi32 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psrad ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrad (__m64 __m, __m64 __count)
{
  return _mm_sra_pi32 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srai_pi32 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psradi ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psradi (__m64 __m, int __count)
{
  return _mm_srai_pi32 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srl_pi16 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psrlw ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrlw (__m64 __m, __m64 __count)
{
  return _mm_srl_pi16 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srli_pi16 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psrlwi ((__v4hi)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrlwi (__m64 __m, int __count)
{
  return _mm_srli_pi16 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srl_pi32 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psrld ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrld (__m64 __m, __m64 __count)
{
  return _mm_srl_pi32 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srli_pi32 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psrldi ((__v2si)__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrldi (__m64 __m, int __count)
{
  return _mm_srli_pi32 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srl_si64 (__m64 __m, __m64 __count)
{

  return (__m64) __builtin_ia32_psrlq (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrlq (__m64 __m, __m64 __count)
{
  return _mm_srl_si64 (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_srli_si64 (__m64 __m, int __count)
{

  return (__m64) __builtin_ia32_psrlqi (__m, __count);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psrlqi (__m64 __m, int __count)
{
  return _mm_srli_si64 (__m, __count);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_and_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_ia32_pand (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pand (__m64 __m1, __m64 __m2)
{
  return _mm_and_si64 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_andnot_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_ia32_pandn (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pandn (__m64 __m1, __m64 __m2)
{
  return _mm_andnot_si64 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_or_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_ia32_por (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_por (__m64 __m1, __m64 __m2)
{
  return _mm_or_si64 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_xor_si64 (__m64 __m1, __m64 __m2)
{
  return __builtin_ia32_pxor (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pxor (__m64 __m1, __m64 __m2)
{
  return _mm_xor_si64 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpeqb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpeqb (__m64 __m1, __m64 __m2)
{
  return _mm_cmpeq_pi8 (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_pi8 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpgtb ((__v8qi)__m1, (__v8qi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpgtb (__m64 __m1, __m64 __m2)
{
  return _mm_cmpgt_pi8 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpeqw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpeqw (__m64 __m1, __m64 __m2)
{
  return _mm_cmpeq_pi16 (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_pi16 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpgtw ((__v4hi)__m1, (__v4hi)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpgtw (__m64 __m1, __m64 __m2)
{
  return _mm_cmpgt_pi16 (__m1, __m2);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpeqd ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpeqd (__m64 __m1, __m64 __m2)
{
  return _mm_cmpeq_pi32 (__m1, __m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_pi32 (__m64 __m1, __m64 __m2)
{
  return (__m64) __builtin_ia32_pcmpgtd ((__v2si)__m1, (__v2si)__m2);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pcmpgtd (__m64 __m1, __m64 __m2)
{
  return _mm_cmpgt_pi32 (__m1, __m2);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_setzero_si64 (void)
{
  return (__m64)0LL;
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set_pi32 (int __i1, int __i0)
{
  return (__m64) __builtin_ia32_vec_init_v2si (__i0, __i1);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set_pi16 (short __w3, short __w2, short __w1, short __w0)
{
  return (__m64) __builtin_ia32_vec_init_v4hi (__w0, __w1, __w2, __w3);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set_pi8 (char __b7, char __b6, char __b5, char __b4,
      char __b3, char __b2, char __b1, char __b0)
{
  return (__m64) __builtin_ia32_vec_init_v8qi (__b0, __b1, __b2, __b3,
            __b4, __b5, __b6, __b7);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_setr_pi32 (int __i0, int __i1)
{
  return _mm_set_pi32 (__i1, __i0);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_setr_pi16 (short __w0, short __w1, short __w2, short __w3)
{
  return _mm_set_pi16 (__w3, __w2, __w1, __w0);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_setr_pi8 (char __b0, char __b1, char __b2, char __b3,
       char __b4, char __b5, char __b6, char __b7)
{
  return _mm_set_pi8 (__b7, __b6, __b5, __b4, __b3, __b2, __b1, __b0);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set1_pi32 (int __i)
{
  return _mm_set_pi32 (__i, __i);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set1_pi16 (short __w)
{
  return _mm_set_pi16 (__w, __w, __w, __w);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_set1_pi8 (char __b)
{
  return _mm_set_pi8 (__b, __b, __b, __b, __b, __b, __b, __b);
}
# 54 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h" 2





# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 1 3 4
# 45 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mm_malloc.h" 1 3 4
# 31 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mm_malloc.h" 3 4
# 1 "/usr/include/errno.h" 1 3 4
# 23 "/usr/include/errno.h" 3 4
# 1 "/usr/include/sys/errno.h" 1 3 4
# 74 "/usr/include/sys/errno.h" 3 4

extern int * __error(void);


# 24 "/usr/include/errno.h" 2 3 4
# 32 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/mm_malloc.h" 2 3 4

static __inline__ void*
_mm_malloc (size_t size, size_t align)
{
  void * malloc_ptr;
  void * aligned_ptr;


  if (align & (align - 1))
    {
      (*__error()) = 22;
      return ((void*) 0);
    }

  if (size == 0)
    return ((void *) 0);





    if (align < 2 * sizeof (void *))
      align = 2 * sizeof (void *);

  malloc_ptr = malloc (size + align);
  if (!malloc_ptr)
    return ((void *) 0);


  aligned_ptr = (void *) (((size_t) malloc_ptr + align)
     & ~((size_t) (align) - 1));


  ((void **) aligned_ptr) [-1] = malloc_ptr;

  return aligned_ptr;
}

static __inline__ void
_mm_free (void * aligned_ptr)
{
  if (aligned_ptr)
    free (((void **) aligned_ptr) [-1]);
}
# 46 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 2 3 4





typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));


typedef float __v4sf __attribute__ ((__vector_size__ (16)));






enum _mm_hint
{
  _MM_HINT_T0 = 3,
  _MM_HINT_T1 = 2,
  _MM_HINT_T2 = 1,
  _MM_HINT_NTA = 0
};
# 112 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_setzero_ps (void)
{
  return __extension__ (__m128){ 0.0f, 0.0f, 0.0f, 0.0f };
}






static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_add_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_addss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_sub_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_subss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_mul_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_mulss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_div_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_divss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_sqrt_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_sqrtss ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_rcp_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_rcpss ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_rsqrt_ss (__m128 __A)
{
  return (__m128) __builtin_ia32_rsqrtss ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_min_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_minss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_max_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_maxss ((__v4sf)__A, (__v4sf)__B);
}




static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_add_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_addps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_sub_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_subps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_mul_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_mulps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_div_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_divps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_sqrt_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_sqrtps ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_rcp_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_rcpps ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_rsqrt_ps (__m128 __A)
{
  return (__m128) __builtin_ia32_rsqrtps ((__v4sf)__A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_min_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_minps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_max_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_maxps ((__v4sf)__A, (__v4sf)__B);
}




static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_and_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_andps (__A, __B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_andnot_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_andnps (__A, __B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_or_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_orps (__A, __B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_xor_ps (__m128 __A, __m128 __B)
{
  return __builtin_ia32_xorps (__A, __B);
}






static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpeqss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpltss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmple_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpless ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
     (__v4sf)
     __builtin_ia32_cmpltss ((__v4sf) __B,
        (__v4sf)
        __A));
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpge_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
     (__v4sf)
     __builtin_ia32_cmpless ((__v4sf) __B,
        (__v4sf)
        __A));
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpneq_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpneqss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnlt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnltss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnle_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnless ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpngt_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
     (__v4sf)
     __builtin_ia32_cmpnltss ((__v4sf) __B,
         (__v4sf)
         __A));
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnge_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf) __A,
     (__v4sf)
     __builtin_ia32_cmpnless ((__v4sf) __B,
         (__v4sf)
         __A));
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpord_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpordss ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpunord_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpunordss ((__v4sf)__A, (__v4sf)__B);
}






static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpeqps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpltps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmple_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpleps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpgtps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpge_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpgeps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpneq_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpneqps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnlt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnltps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnle_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpnleps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpngt_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpngtps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpnge_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpngeps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpord_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpordps ((__v4sf)__A, (__v4sf)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cmpunord_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_cmpunordps ((__v4sf)__A, (__v4sf)__B);
}





static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comieq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comieq ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comilt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comilt ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comile_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comile ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comigt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comigt ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comige_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comige ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comineq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_comineq ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomieq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomieq ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomilt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomilt ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomile_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomile ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomigt_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomigt ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomige_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomige ((__v4sf)__A, (__v4sf)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomineq_ss (__m128 __A, __m128 __B)
{
  return __builtin_ia32_ucomineq ((__v4sf)__A, (__v4sf)__B);
}




static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvtss_si32 (__m128 __A)
{
  return __builtin_ia32_cvtss2si ((__v4sf) __A);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvt_ss2si (__m128 __A)
{
  return _mm_cvtss_si32 (__A);
}







static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtss_si64 (__m128 __A)
{
  return __builtin_ia32_cvtss2si64 ((__v4sf) __A);
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtss_si64x (__m128 __A)
{
  return __builtin_ia32_cvtss2si64 ((__v4sf) __A);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtps_pi32 (__m128 __A)
{
  return (__m64) __builtin_ia32_cvtps2pi ((__v4sf) __A);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvt_ps2pi (__m128 __A)
{
  return _mm_cvtps_pi32 (__A);
}



static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvttss_si32 (__m128 __A)
{
  return __builtin_ia32_cvttss2si ((__v4sf) __A);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvtt_ss2si (__m128 __A)
{
  return _mm_cvttss_si32 (__A);
}






static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvttss_si64 (__m128 __A)
{
  return __builtin_ia32_cvttss2si64 ((__v4sf) __A);
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvttss_si64x (__m128 __A)
{
  return __builtin_ia32_cvttss2si64 ((__v4sf) __A);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvttps_pi32 (__m128 __A)
{
  return (__m64) __builtin_ia32_cvttps2pi ((__v4sf) __A);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtt_ps2pi (__m128 __A)
{
  return _mm_cvttps_pi32 (__A);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi32_ss (__m128 __A, int __B)
{
  return (__m128) __builtin_ia32_cvtsi2ss ((__v4sf) __A, __B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvt_si2ss (__m128 __A, int __B)
{
  return _mm_cvtsi32_ss (__A, __B);
}






static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_ss (__m128 __A, long long __B)
{
  return (__m128) __builtin_ia32_cvtsi642ss ((__v4sf) __A, __B);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64x_ss (__m128 __A, long long __B)
{
  return (__m128) __builtin_ia32_cvtsi642ss ((__v4sf) __A, __B);
}





static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpi32_ps (__m128 __A, __m64 __B)
{
  return (__m128) __builtin_ia32_cvtpi2ps ((__v4sf) __A, (__v2si)__B);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvt_pi2ps (__m128 __A, __m64 __B)
{
  return _mm_cvtpi32_ps (__A, __B);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpi16_ps (__m64 __A)
{
  __v4hi __sign;
  __v2si __hisi, __losi;
  __v4sf __r;




  __sign = __builtin_ia32_pcmpgtw ((__v4hi)0LL, (__v4hi)__A);


  __hisi = (__v2si) __builtin_ia32_punpckhwd ((__v4hi)__A, __sign);
  __losi = (__v2si) __builtin_ia32_punpcklwd ((__v4hi)__A, __sign);


  __r = (__v4sf) _mm_setzero_ps ();
  __r = __builtin_ia32_cvtpi2ps (__r, __hisi);
  __r = __builtin_ia32_movlhps (__r, __r);
  __r = __builtin_ia32_cvtpi2ps (__r, __losi);

  return (__m128) __r;
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpu16_ps (__m64 __A)
{
  __v2si __hisi, __losi;
  __v4sf __r;


  __hisi = (__v2si) __builtin_ia32_punpckhwd ((__v4hi)__A, (__v4hi)0LL);
  __losi = (__v2si) __builtin_ia32_punpcklwd ((__v4hi)__A, (__v4hi)0LL);


  __r = (__v4sf) _mm_setzero_ps ();
  __r = __builtin_ia32_cvtpi2ps (__r, __hisi);
  __r = __builtin_ia32_movlhps (__r, __r);
  __r = __builtin_ia32_cvtpi2ps (__r, __losi);

  return (__m128) __r;
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpi8_ps (__m64 __A)
{
  __v8qi __sign;




  __sign = __builtin_ia32_pcmpgtb ((__v8qi)0LL, (__v8qi)__A);


  __A = (__m64) __builtin_ia32_punpcklbw ((__v8qi)__A, __sign);

  return _mm_cvtpi16_ps(__A);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpu8_ps(__m64 __A)
{
  __A = (__m64) __builtin_ia32_punpcklbw ((__v8qi)__A, (__v8qi)0LL);
  return _mm_cvtpu16_ps(__A);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpi32x2_ps(__m64 __A, __m64 __B)
{
  __v4sf __zero = (__v4sf) _mm_setzero_ps ();
  __v4sf __sfa = __builtin_ia32_cvtpi2ps (__zero, (__v2si)__A);
  __v4sf __sfb = __builtin_ia32_cvtpi2ps (__zero, (__v2si)__B);
  return (__m128) __builtin_ia32_movlhps (__sfa, __sfb);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtps_pi16(__m128 __A)
{
  __v4sf __hisf = (__v4sf)__A;
  __v4sf __losf = __builtin_ia32_movhlps (__hisf, __hisf);
  __v2si __hisi = __builtin_ia32_cvtps2pi (__hisf);
  __v2si __losi = __builtin_ia32_cvtps2pi (__losf);
  return (__m64) __builtin_ia32_packssdw (__hisi, __losi);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtps_pi8(__m128 __A)
{
  __v4hi __tmp = (__v4hi) _mm_cvtps_pi16 (__A);
  return (__m64) __builtin_ia32_packsswb (__tmp, (__v4hi)0LL);
}
# 922 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_unpckhps ((__v4sf)__A, (__v4sf)__B);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_unpcklps ((__v4sf)__A, (__v4sf)__B);
}




static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_loadh_pi (__m128 __A, __m64 const *__P)
{
  return (__m128) __builtin_ia32_loadhps ((__v4sf)__A, (__v2si *)__P);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storeh_pi (__m64 *__P, __m128 __A)
{
  __builtin_ia32_storehps ((__v2si *)__P, (__v4sf)__A);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_movehl_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movhlps ((__v4sf)__A, (__v4sf)__B);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_movelh_ps (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movlhps ((__v4sf)__A, (__v4sf)__B);
}




static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_loadl_pi (__m128 __A, __m64 const *__P)
{
  return (__m128) __builtin_ia32_loadlps ((__v4sf)__A, (__v2si *)__P);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storel_pi (__m64 *__P, __m128 __A)
{
  __builtin_ia32_storelps ((__v2si *)__P, (__v4sf)__A);
}



static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_movemask_ps (__m128 __A)
{
  return __builtin_ia32_movmskps ((__v4sf)__A);
}



static __inline unsigned int __attribute__((__always_inline__, __nodebug__))

_mm_getcsr (void)
{
  return __builtin_ia32_stmxcsr ();
}



static __inline unsigned int __attribute__((__always_inline__, __nodebug__))

_MM_GET_EXCEPTION_STATE (void)
{
  return _mm_getcsr() & 0x003f;
}


static __inline unsigned int __attribute__((__always_inline__, __nodebug__))

_MM_GET_EXCEPTION_MASK (void)
{
  return _mm_getcsr() & 0x1f80;
}


static __inline unsigned int __attribute__((__always_inline__, __nodebug__))

_MM_GET_ROUNDING_MODE (void)
{
  return _mm_getcsr() & 0x6000;
}


static __inline unsigned int __attribute__((__always_inline__, __nodebug__))

_MM_GET_FLUSH_ZERO_MODE (void)
{
  return _mm_getcsr() & 0x8000;
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_setcsr (unsigned int __I)
{
  __builtin_ia32_ldmxcsr (__I);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_MM_SET_EXCEPTION_STATE(unsigned int __mask)
{
  _mm_setcsr((_mm_getcsr() & ~0x003f) | __mask);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_MM_SET_EXCEPTION_MASK (unsigned int __mask)
{
  _mm_setcsr((_mm_getcsr() & ~0x1f80) | __mask);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_MM_SET_ROUNDING_MODE (unsigned int __mode)
{
  _mm_setcsr((_mm_getcsr() & ~0x6000) | __mode);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_MM_SET_FLUSH_ZERO_MODE (unsigned int __mode)
{
  _mm_setcsr((_mm_getcsr() & ~0x8000) | __mode);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_set_ss (float __F)
{
  return __extension__ (__m128)(__v4sf){ __F, 0, 0, 0 };
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_set1_ps (float __F)
{
  return __extension__ (__m128)(__v4sf){ __F, __F, __F, __F };
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_set_ps1 (float __F)
{
  return _mm_set1_ps (__F);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_load_ss (float const *__P)
{
  return _mm_set_ss (*__P);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_load1_ps (float const *__P)
{
  return _mm_set1_ps (*__P);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_load_ps1 (float const *__P)
{
  return _mm_load1_ps (__P);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_load_ps (float const *__P)
{
  return (__m128) *(__v4sf *)__P;
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_loadu_ps (float const *__P)
{
  return (__m128) __builtin_ia32_loadups (__P);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_loadr_ps (float const *__P)
{
  __v4sf __tmp = *(__v4sf *)__P;
  return (__m128) __builtin_ia32_shufps (__tmp, __tmp, (((0) << 6) | ((1) << 4) | ((2) << 2) | (3)));
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_set_ps (const float __Z, const float __Y, const float __X, const float __W)
{
  return __extension__ (__m128)(__v4sf){ __W, __X, __Y, __Z };
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_setr_ps (float __Z, float __Y, float __X, float __W)
{
  return __extension__ (__m128)(__v4sf){ __Z, __Y, __X, __W };
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_ss (float *__P, __m128 __A)
{
  *__P = __builtin_ia32_vec_ext_v4sf ((__v4sf)__A, 0);
}


static __inline float __attribute__((__always_inline__, __nodebug__))

_mm_cvtss_f32 (__m128 __A)
{
  return __builtin_ia32_vec_ext_v4sf ((__v4sf)__A, 0);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_ps (float *__P, __m128 __A)
{
  *(__v4sf *)__P = (__v4sf)__A;
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storeu_ps (float *__P, __m128 __A)
{
  __builtin_ia32_storeups (__P, (__v4sf)__A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store1_ps (float *__P, __m128 __A)
{
  __v4sf __va = (__v4sf)__A;
  __v4sf __tmp = __builtin_ia32_shufps (__va, __va, (((0) << 6) | ((0) << 4) | ((0) << 2) | (0)));
  _mm_storeu_ps (__P, __tmp);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_ps1 (float *__P, __m128 __A)
{
  _mm_store1_ps (__P, __A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storer_ps (float *__P, __m128 __A)
{
  __v4sf __va = (__v4sf)__A;
  __v4sf __tmp = __builtin_ia32_shufps (__va, __va, (((0) << 6) | ((1) << 4) | ((2) << 2) | (3)));
  _mm_store_ps (__P, __tmp);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_move_ss (__m128 __A, __m128 __B)
{
  return (__m128) __builtin_ia32_movss ((__v4sf)__A, (__v4sf)__B);
}
# 1307 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_max_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmaxsw ((__v4hi)__A, (__v4hi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmaxsw (__m64 __A, __m64 __B)
{
  return _mm_max_pi16 (__A, __B);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_max_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmaxub ((__v8qi)__A, (__v8qi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmaxub (__m64 __A, __m64 __B)
{
  return _mm_max_pu8 (__A, __B);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_min_pi16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pminsw ((__v4hi)__A, (__v4hi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pminsw (__m64 __A, __m64 __B)
{
  return _mm_min_pi16 (__A, __B);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_min_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pminub ((__v8qi)__A, (__v8qi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pminub (__m64 __A, __m64 __B)
{
  return _mm_min_pu8 (__A, __B);
}



static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_movemask_pi8 (__m64 __A)
{
  return __builtin_ia32_pmovmskb ((__v8qi)__A);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_m_pmovmskb (__m64 __A)
{
  return _mm_movemask_pi8 (__A);
}




static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_mulhi_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pmulhuw ((__v4hi)__A, (__v4hi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pmulhuw (__m64 __A, __m64 __B)
{
  return _mm_mulhi_pu16 (__A, __B);
}
# 1436 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_maskmove_si64 (__m64 __A, __m64 __N, char *__P)
{
  __builtin_ia32_maskmovq ((__v8qi)__A, (__v8qi)__N, __P);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_m_maskmovq (__m64 __A, __m64 __N, char *__P)
{
  _mm_maskmove_si64 (__A, __N, __P);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_avg_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pavgb ((__v8qi)__A, (__v8qi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pavgb (__m64 __A, __m64 __B)
{
  return _mm_avg_pu8 (__A, __B);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_avg_pu16 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_pavgw ((__v4hi)__A, (__v4hi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_pavgw (__m64 __A, __m64 __B)
{
  return _mm_avg_pu16 (__A, __B);
}





static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_sad_pu8 (__m64 __A, __m64 __B)
{
  return (__m64) __builtin_ia32_psadbw ((__v8qi)__A, (__v8qi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_m_psadbw (__m64 __A, __m64 __B)
{
  return _mm_sad_pu8 (__A, __B);
}
# 1521 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_stream_pi (__m64 *__P, __m64 __A)
{

  __builtin_ia32_movntq (__P, __A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_stream_ps (float *__P, __m128 __A)
{
  __builtin_ia32_movntps (__P, (__v4sf)__A);
}




static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_sfence (void)
{
  __builtin_ia32_sfence ();
}





static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_pause (void)
{
  __asm__ __volatile__ ("rep; nop" : : );
}
# 1579 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 3 4
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 1 3 4
# 35 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
# 1 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 1 3 4
# 36 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 2 3 4


typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));



typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));
typedef double __m128d __attribute__ ((__vector_size__ (16), __may_alias__));
# 69 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_set_sd (double __F)
{
  return __extension__ (__m128d){ __F, 0 };
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_set1_pd (double __F)
{
  return __extension__ (__m128d){ __F, __F };
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_set_pd1 (double __F)
{
  return _mm_set1_pd (__F);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_set_pd (double __W, double __X)
{
  return __extension__ (__m128d){ __X, __W };
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_setr_pd (double __W, double __X)
{
  return __extension__ (__m128d){ __W, __X };
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_setzero_pd (void)
{
  return __extension__ (__m128d){ 0.0, 0.0 };
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_move_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df)__A, (__v2df)__B);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_load_pd (double const *__P)
{
  return *(__m128d *)__P;
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_loadu_pd (double const *__P)
{
  return __builtin_ia32_loadupd (__P);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_load1_pd (double const *__P)
{
  return _mm_set1_pd (*__P);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_load_sd (double const *__P)
{
  return _mm_set_sd (*__P);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_load_pd1 (double const *__P)
{
  return _mm_load1_pd (__P);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_loadr_pd (double const *__P)
{
  __m128d __tmp = _mm_load_pd (__P);
  return __builtin_ia32_shufpd (__tmp, __tmp, (((0) << 1) | (1)));
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_pd (double *__P, __m128d __A)
{
  *(__m128d *)__P = __A;
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storeu_pd (double *__P, __m128d __A)
{
  __builtin_ia32_storeupd (__P, __A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_sd (double *__P, __m128d __A)
{
  *__P = __builtin_ia32_vec_ext_v2df (__A, 0);
}


static __inline double __attribute__((__always_inline__, __nodebug__))

_mm_cvtsd_f64 (__m128d __A)
{
  return __builtin_ia32_vec_ext_v2df (__A, 0);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storel_pd (double *__P, __m128d __A)
{
  _mm_store_sd (__P, __A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storeh_pd (double *__P, __m128d __A)
{
  *__P = __builtin_ia32_vec_ext_v2df (__A, 1);
}




static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store1_pd (double *__P, __m128d __A)
{
  _mm_store_pd (__P, __builtin_ia32_shufpd (__A, __A, (((0) << 1) | (0))));
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_pd1 (double *__P, __m128d __A)
{
  _mm_store1_pd (__P, __A);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storer_pd (double *__P, __m128d __A)
{
  _mm_store_pd (__P, __builtin_ia32_shufpd (__A, __A, (((0) << 1) | (1))));
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi128_si32 (__m128i __A)
{
  return __builtin_ia32_vec_ext_v4si ((__v4si)__A, 0);
}




static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi128_si64 (__m128i __A)
{
  return __builtin_ia32_vec_ext_v2di ((__v2di)__A, 0);
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi128_si64x (__m128i __A)
{
  return __builtin_ia32_vec_ext_v2di ((__v2di)__A, 0);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_add_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_addpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_add_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_addsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_sub_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_subpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_sub_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_subsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_mul_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_mulpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_mul_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_mulsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_div_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_divpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_div_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_divsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_sqrt_pd (__m128d __A)
{
  return (__m128d)__builtin_ia32_sqrtpd ((__v2df)__A);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_sqrt_sd (__m128d __A, __m128d __B)
{
  __v2df __tmp = __builtin_ia32_movsd ((__v2df)__A, (__v2df)__B);
  return (__m128d)__builtin_ia32_sqrtsd ((__v2df)__tmp);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_min_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_minpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_min_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_minsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_max_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_maxpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_max_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_maxsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_and_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_andpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_andnot_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_andnpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_or_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_orpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_xor_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_xorpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpeqpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpltpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmple_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmplepd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpgtpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpge_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpgepd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpneq_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpneqpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnlt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnltpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnle_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnlepd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpngt_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpngtpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnge_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpngepd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpord_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpordpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpunord_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpunordpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpeqsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpltsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmple_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmplesd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
      (__v2df)
      __builtin_ia32_cmpltsd ((__v2df) __B,
         (__v2df)
         __A));
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpge_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
      (__v2df)
      __builtin_ia32_cmplesd ((__v2df) __B,
         (__v2df)
         __A));
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpneq_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpneqsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnlt_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnltsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnle_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpnlesd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpngt_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
      (__v2df)
      __builtin_ia32_cmpnltsd ((__v2df) __B,
          (__v2df)
          __A));
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpnge_sd (__m128d __A, __m128d __B)
{
  return (__m128d) __builtin_ia32_movsd ((__v2df) __A,
      (__v2df)
      __builtin_ia32_cmpnlesd ((__v2df) __B,
          (__v2df)
          __A));
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpord_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpordsd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cmpunord_sd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_cmpunordsd ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comieq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdeq ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comilt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdlt ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comile_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdle ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comigt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdgt ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comige_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdge ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_comineq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_comisdneq ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomieq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdeq ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomilt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdlt ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomile_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdle ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomigt_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdgt ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomige_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdge ((__v2df)__A, (__v2df)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_ucomineq_sd (__m128d __A, __m128d __B)
{
  return __builtin_ia32_ucomisdneq ((__v2df)__A, (__v2df)__B);
}




static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set_epi64x (long long __q1, long long __q0)
{
  return __extension__ (__m128i)(__v2di){ __q0, __q1 };
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set_epi64 (__m64 __q1, __m64 __q0)
{
  return _mm_set_epi64x ((long long)__q1, (long long)__q0);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set_epi32 (int __q3, int __q2, int __q1, int __q0)
{
  return __extension__ (__m128i)(__v4si){ __q0, __q1, __q2, __q3 };
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set_epi16 (short __q7, short __q6, short __q5, short __q4,
        short __q3, short __q2, short __q1, short __q0)
{
  return __extension__ (__m128i)(__v8hi){
    __q0, __q1, __q2, __q3, __q4, __q5, __q6, __q7 };
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set_epi8 (char __q15, char __q14, char __q13, char __q12,
       char __q11, char __q10, char __q09, char __q08,
       char __q07, char __q06, char __q05, char __q04,
       char __q03, char __q02, char __q01, char __q00)
{
  return __extension__ (__m128i)(__v16qi){
    __q00, __q01, __q02, __q03, __q04, __q05, __q06, __q07,
    __q08, __q09, __q10, __q11, __q12, __q13, __q14, __q15
  };
}
# 798 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_setr_epi64 (__m64 __q0, __m64 __q1)
{
  return _mm_set_epi64 (__q1, __q0);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_setr_epi32 (int __q0, int __q1, int __q2, int __q3)
{
  return _mm_set_epi32 (__q3, __q2, __q1, __q0);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_setr_epi16 (short __q0, short __q1, short __q2, short __q3,
         short __q4, short __q5, short __q6, short __q7)
{
  return _mm_set_epi16 (__q7, __q6, __q5, __q4, __q3, __q2, __q1, __q0);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_setr_epi8 (char __q00, char __q01, char __q02, char __q03,
        char __q04, char __q05, char __q06, char __q07,
        char __q08, char __q09, char __q10, char __q11,
        char __q12, char __q13, char __q14, char __q15)
{
  return _mm_set_epi8 (__q15, __q14, __q13, __q12, __q11, __q10, __q09, __q08,
         __q07, __q06, __q05, __q04, __q03, __q02, __q01, __q00);
}




static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_load_si128 (__m128i const *__P)
{
  return *__P;
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_loadu_si128 (__m128i const *__P)
{
  return (__m128i) __builtin_ia32_loaddqu ((char const *)__P);
}



static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_loadl_epi64 (__m128i const *__P)
{
  return (__m128i)__builtin_ia32_loadlv4si ((__v2si *)__P);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_store_si128 (__m128i *__P, __m128i __B)
{
  *__P = __B;
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storeu_si128 (__m128i *__P, __m128i __B)
{
  __builtin_ia32_storedqu ((char *)__P, (__v16qi)__B);
}



static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_storel_epi64 (__m128i *__P, __m128i __B)
{
  __builtin_ia32_storelv4si ((__v2si *)__P, __B);
}



static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_movepi64_pi64 (__m128i __B)
{
  return (__m64) __builtin_ia32_vec_ext_v2di ((__v2di)__B, 0);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_movpi64_epi64 (__m64 __A)
{
  return _mm_set_epi64 ((__m64)0LL, __A);
}



static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_move_epi64 (__m128i __A)
{
  return (__m128i)__builtin_ia32_movqv4si ((__v4si)__A) ;
}




static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_setzero_si128 (void)
{
  return __extension__ (__m128i)(__v4si){ 0, 0, 0, 0 };
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtepi32_pd (__m128i __A)
{
  return (__m128d)__builtin_ia32_cvtdq2pd ((__v4si) __A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtepi32_ps (__m128i __A)
{
  return (__m128)__builtin_ia32_cvtdq2ps ((__v4si) __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvtpd_epi32 (__m128d __A)
{
  return (__m128i)__builtin_ia32_cvtpd2dq ((__v2df) __A);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpd_pi32 (__m128d __A)
{
  return (__m64)__builtin_ia32_cvtpd2pi ((__v2df) __A);
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtpd_ps (__m128d __A)
{
  return (__m128)__builtin_ia32_cvtpd2ps ((__v2df) __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvttpd_epi32 (__m128d __A)
{
  return (__m128i)__builtin_ia32_cvttpd2dq ((__v2df) __A);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_cvttpd_pi32 (__m128d __A)
{
  return (__m64)__builtin_ia32_cvttpd2pi ((__v2df) __A);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtpi32_pd (__m64 __A)
{
  return (__m128d)__builtin_ia32_cvtpi2pd ((__v2si) __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvtps_epi32 (__m128 __A)
{
  return (__m128i)__builtin_ia32_cvtps2dq ((__v4sf) __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvttps_epi32 (__m128 __A)
{
  return (__m128i)__builtin_ia32_cvttps2dq ((__v4sf) __A);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtps_pd (__m128 __A)
{
  return (__m128d)__builtin_ia32_cvtps2pd ((__v4sf) __A);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvtsd_si32 (__m128d __A)
{
  return __builtin_ia32_cvtsd2si ((__v2df) __A);
}




static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtsd_si64 (__m128d __A)
{
  return __builtin_ia32_cvtsd2si64 ((__v2df) __A);
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvtsd_si64x (__m128d __A)
{
  return __builtin_ia32_cvtsd2si64 ((__v2df) __A);
}



static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_cvttsd_si32 (__m128d __A)
{
  return __builtin_ia32_cvttsd2si ((__v2df) __A);
}




static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvttsd_si64 (__m128d __A)
{
  return __builtin_ia32_cvttsd2si64 ((__v2df) __A);
}



static __inline long long __attribute__((__always_inline__, __nodebug__))

_mm_cvttsd_si64x (__m128d __A)
{
  return __builtin_ia32_cvttsd2si64 ((__v2df) __A);
}



static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_cvtsd_ss (__m128 __A, __m128d __B)
{
  return (__m128)__builtin_ia32_cvtsd2ss ((__v4sf) __A, (__v2df) __B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi32_sd (__m128d __A, int __B)
{
  return (__m128d)__builtin_ia32_cvtsi2sd ((__v2df) __A, __B);
}




static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_sd (__m128d __A, long long __B)
{
  return (__m128d)__builtin_ia32_cvtsi642sd ((__v2df) __A, __B);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64x_sd (__m128d __A, long long __B)
{
  return (__m128d)__builtin_ia32_cvtsi642sd ((__v2df) __A, __B);
}



static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_cvtss_sd (__m128d __A, __m128 __B)
{
  return (__m128d)__builtin_ia32_cvtss2sd ((__v2df) __A, (__v4sf)__B);
}





static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_unpckhpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_pd (__m128d __A, __m128d __B)
{
  return (__m128d)__builtin_ia32_unpcklpd ((__v2df)__A, (__v2df)__B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_loadh_pd (__m128d __A, double const *__B)
{
  return (__m128d)__builtin_ia32_loadhpd ((__v2df)__A, __B);
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_loadl_pd (__m128d __A, double const *__B)
{
  return (__m128d)__builtin_ia32_loadlpd ((__v2df)__A, __B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_movemask_pd (__m128d __A)
{
  return __builtin_ia32_movmskpd ((__v2df)__A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_packs_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packsswb128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_packs_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packssdw128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_packus_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_packuswb128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhbw128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhwd128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhdq128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpackhi_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckhqdq128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklbw128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklwd128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpckldq128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_unpacklo_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_punpcklqdq128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_add_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_add_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_add_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddd128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_add_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddq128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_adds_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddsb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_adds_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddsw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_adds_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddusb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_adds_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_paddusw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sub_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sub_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sub_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubd128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sub_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubq128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_subs_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubsb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_subs_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubsw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_subs_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubusb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_subs_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psubusw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_madd_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaddwd128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_mulhi_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmulhw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_mullo_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmullw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m64 __attribute__((__always_inline__, __nodebug__))

_mm_mul_su32 (__m64 __A, __m64 __B)
{
  return (__m64)__builtin_ia32_pmuludq ((__v2si)__A, (__v2si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_mul_epu32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmuludq128 ((__v4si)__A, (__v4si)__B);
}
# 1519 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sll_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psllw128((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sll_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pslld128((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sll_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psllq128((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sra_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psraw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sra_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrad128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_srl_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrlw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_srl_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrld128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_srl_epi64 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psrlq128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_and_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pand128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_andnot_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pandn128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_or_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_por128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_xor_si128 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pxor128 ((__v2di)__A, (__v2di)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpeq_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpeqd128 ((__v4si)__A, (__v4si)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtb128 ((__v16qi)__B, (__v16qi)__A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtw128 ((__v8hi)__B, (__v8hi)__A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmplt_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtd128 ((__v4si)__B, (__v4si)__A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_epi8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cmpgt_epi32 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pcmpgtd128 ((__v4si)__A, (__v4si)__B);
}
# 1710 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_max_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaxsw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_max_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmaxub128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_min_epi16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pminsw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_min_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pminub128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline int __attribute__((__always_inline__, __nodebug__))

_mm_movemask_epi8 (__m128i __A)
{
  return __builtin_ia32_pmovmskb128 ((__v16qi)__A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_mulhi_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pmulhuw128 ((__v8hi)__A, (__v8hi)__B);
}
# 1764 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/emmintrin.h" 3 4
static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_maskmoveu_si128 (__m128i __A, __m128i __B, char *__C)
{
  __builtin_ia32_maskmovdqu ((__v16qi)__A, (__v16qi)__B, __C);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_avg_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pavgb128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_avg_epu16 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_pavgw128 ((__v8hi)__A, (__v8hi)__B);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_sad_epu8 (__m128i __A, __m128i __B)
{
  return (__m128i)__builtin_ia32_psadbw128 ((__v16qi)__A, (__v16qi)__B);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_stream_si32 (int *__A, int __B)
{
  __builtin_ia32_movnti (__A, __B);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_stream_si128 (__m128i *__A, __m128i __B)
{
  __builtin_ia32_movntdq ((__v2di *)__A, (__v2di)__B);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_stream_pd (double *__A, __m128d __B)
{
  __builtin_ia32_movntpd (__A, (__v2df)__B);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_clflush (void const *__A)
{
  __builtin_ia32_clflush (__A);
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_lfence (void)
{
  __builtin_ia32_lfence ();
}


static __inline void __attribute__((__always_inline__, __nodebug__))

_mm_mfence (void)
{
  __builtin_ia32_mfence ();
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi32_si128 (int __A)
{
  return _mm_set_epi32 (0, 0, 0, __A);
}




static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64_si128 (long long __A)
{
  return _mm_set_epi64x (0, __A);
}



static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_cvtsi64x_si128 (long long __A)
{
  return _mm_set_epi64x (0, __A);
}





static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_castpd_ps(__m128d __A)
{
  return (__m128) __A;
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_castpd_si128(__m128d __A)
{
  return (__m128i) __A;
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_castps_pd(__m128 __A)
{
  return (__m128d) __A;
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_castps_si128(__m128 __A)
{
  return (__m128i) __A;
}


static __inline __m128 __attribute__((__always_inline__, __nodebug__))

_mm_castsi128_ps(__m128i __A)
{
  return (__m128) __A;
}


static __inline __m128d __attribute__((__always_inline__, __nodebug__))

_mm_castsi128_pd(__m128i __A)
{
  return (__m128d) __A;
}






static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set1_epi64x (long long __A)
{
  return _mm_set_epi64x (__A, __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set1_epi64 (__m64 __A)
{
  return _mm_set_epi64 (__A, __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set1_epi32 (int __A)
{
  return _mm_set_epi32 (__A, __A, __A, __A);
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set1_epi16 (short __A)
{
  __m128i temp, temp2, temp3;
  temp = _mm_cvtsi32_si128((int)__A);
  temp2 = _mm_unpacklo_epi16(temp, temp);
  temp3 = ((__m128i)__builtin_ia32_pshufd ((__v4si)(temp2), (0)));
  return temp3;
}


static __inline __m128i __attribute__((__always_inline__, __nodebug__))

_mm_set1_epi8 (char __A)
{
    __m128i temp, temp2, temp3, temp4;
    temp = _mm_cvtsi32_si128 ((int)__A);
    temp2 = _mm_unpacklo_epi8 (temp, temp);
    temp3 = _mm_unpacklo_epi8 (temp2, temp2);
    temp4 = ((__m128i)__builtin_ia32_pshufd ((__v4si)(temp3), (0)));
    return temp4;
}
# 1580 "/usr/llvm-gcc-4.2/bin/../lib/gcc/i686-apple-darwin11/4.2.1/include/xmmintrin.h" 2 3 4
# 60 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h" 2






# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 67 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h" 2
# 84 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h"
extern __attribute__ ((visibility("default"))) int SDL_GetCPUCount(void);







extern __attribute__ ((visibility("default"))) int SDL_GetCPUCacheLineSize(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasRDTSC(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasAltiVec(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasMMX(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_Has3DNow(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasSSE(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasSSE2(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasSSE3(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasSSE41(void);




extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasSSE42(void);
# 146 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 147 "../Frameworks/SDL.framework/Headers/SDL_cpuinfo.h" 2
# 79 "../Frameworks/SDL.framework/Headers/SDL.h" 2


# 1 "../Frameworks/SDL.framework/Headers/SDL_events.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_events.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_video.h" 1
# 32 "../Frameworks/SDL.framework/Headers/SDL_video.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_pixels.h" 1
# 31 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 32 "../Frameworks/SDL.framework/Headers/SDL_pixels.h" 2
# 50 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
enum
{
    SDL_PIXELTYPE_UNKNOWN,
    SDL_PIXELTYPE_INDEX1,
    SDL_PIXELTYPE_INDEX4,
    SDL_PIXELTYPE_INDEX8,
    SDL_PIXELTYPE_PACKED8,
    SDL_PIXELTYPE_PACKED16,
    SDL_PIXELTYPE_PACKED32,
    SDL_PIXELTYPE_ARRAYU8,
    SDL_PIXELTYPE_ARRAYU16,
    SDL_PIXELTYPE_ARRAYU32,
    SDL_PIXELTYPE_ARRAYF16,
    SDL_PIXELTYPE_ARRAYF32
};


enum
{
    SDL_BITMAPORDER_NONE,
    SDL_BITMAPORDER_4321,
    SDL_BITMAPORDER_1234
};


enum
{
    SDL_PACKEDORDER_NONE,
    SDL_PACKEDORDER_XRGB,
    SDL_PACKEDORDER_RGBX,
    SDL_PACKEDORDER_ARGB,
    SDL_PACKEDORDER_RGBA,
    SDL_PACKEDORDER_XBGR,
    SDL_PACKEDORDER_BGRX,
    SDL_PACKEDORDER_ABGR,
    SDL_PACKEDORDER_BGRA
};


enum
{
    SDL_ARRAYORDER_NONE,
    SDL_ARRAYORDER_RGB,
    SDL_ARRAYORDER_RGBA,
    SDL_ARRAYORDER_ARGB,
    SDL_ARRAYORDER_BGR,
    SDL_ARRAYORDER_BGRA,
    SDL_ARRAYORDER_ABGR
};


enum
{
    SDL_PACKEDLAYOUT_NONE,
    SDL_PACKEDLAYOUT_332,
    SDL_PACKEDLAYOUT_4444,
    SDL_PACKEDLAYOUT_1555,
    SDL_PACKEDLAYOUT_5551,
    SDL_PACKEDLAYOUT_565,
    SDL_PACKEDLAYOUT_8888,
    SDL_PACKEDLAYOUT_2101010,
    SDL_PACKEDLAYOUT_1010102
};
# 147 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
enum
{
    SDL_PIXELFORMAT_UNKNOWN,
    SDL_PIXELFORMAT_INDEX1LSB =
        ((1 << 31) | ((SDL_PIXELTYPE_INDEX1) << 24) | ((SDL_BITMAPORDER_4321) << 20) | ((0) << 16) | ((1) << 8) | ((0) << 0)),

    SDL_PIXELFORMAT_INDEX1MSB =
        ((1 << 31) | ((SDL_PIXELTYPE_INDEX1) << 24) | ((SDL_BITMAPORDER_1234) << 20) | ((0) << 16) | ((1) << 8) | ((0) << 0)),

    SDL_PIXELFORMAT_INDEX4LSB =
        ((1 << 31) | ((SDL_PIXELTYPE_INDEX4) << 24) | ((SDL_BITMAPORDER_4321) << 20) | ((0) << 16) | ((4) << 8) | ((0) << 0)),

    SDL_PIXELFORMAT_INDEX4MSB =
        ((1 << 31) | ((SDL_PIXELTYPE_INDEX4) << 24) | ((SDL_BITMAPORDER_1234) << 20) | ((0) << 16) | ((4) << 8) | ((0) << 0)),

    SDL_PIXELFORMAT_INDEX8 =
        ((1 << 31) | ((SDL_PIXELTYPE_INDEX8) << 24) | ((0) << 20) | ((0) << 16) | ((8) << 8) | ((1) << 0)),
    SDL_PIXELFORMAT_RGB332 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED8) << 24) | ((SDL_PACKEDORDER_XRGB) << 20) | ((SDL_PACKEDLAYOUT_332) << 16) | ((8) << 8) | ((1) << 0)),

    SDL_PIXELFORMAT_RGB444 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_XRGB) << 20) | ((SDL_PACKEDLAYOUT_4444) << 16) | ((12) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_RGB555 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_XRGB) << 20) | ((SDL_PACKEDLAYOUT_1555) << 16) | ((15) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_BGR555 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_XBGR) << 20) | ((SDL_PACKEDLAYOUT_1555) << 16) | ((15) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_ARGB4444 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_ARGB) << 20) | ((SDL_PACKEDLAYOUT_4444) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_RGBA4444 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_RGBA) << 20) | ((SDL_PACKEDLAYOUT_4444) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_ABGR4444 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_ABGR) << 20) | ((SDL_PACKEDLAYOUT_4444) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_BGRA4444 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_BGRA) << 20) | ((SDL_PACKEDLAYOUT_4444) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_ARGB1555 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_ARGB) << 20) | ((SDL_PACKEDLAYOUT_1555) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_RGBA5551 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_RGBA) << 20) | ((SDL_PACKEDLAYOUT_5551) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_ABGR1555 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_ABGR) << 20) | ((SDL_PACKEDLAYOUT_1555) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_BGRA5551 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_BGRA) << 20) | ((SDL_PACKEDLAYOUT_5551) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_RGB565 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_XRGB) << 20) | ((SDL_PACKEDLAYOUT_565) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_BGR565 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED16) << 24) | ((SDL_PACKEDORDER_XBGR) << 20) | ((SDL_PACKEDLAYOUT_565) << 16) | ((16) << 8) | ((2) << 0)),

    SDL_PIXELFORMAT_RGB24 =
        ((1 << 31) | ((SDL_PIXELTYPE_ARRAYU8) << 24) | ((SDL_ARRAYORDER_RGB) << 20) | ((0) << 16) | ((24) << 8) | ((3) << 0)),

    SDL_PIXELFORMAT_BGR24 =
        ((1 << 31) | ((SDL_PIXELTYPE_ARRAYU8) << 24) | ((SDL_ARRAYORDER_BGR) << 20) | ((0) << 16) | ((24) << 8) | ((3) << 0)),

    SDL_PIXELFORMAT_RGB888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_XRGB) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((24) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_RGBX8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_RGBX) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((24) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_BGR888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_XBGR) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((24) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_BGRX8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_BGRX) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((24) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_ARGB8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_ARGB) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((32) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_RGBA8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_RGBA) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((32) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_ABGR8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_ABGR) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((32) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_BGRA8888 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_BGRA) << 20) | ((SDL_PACKEDLAYOUT_8888) << 16) | ((32) << 8) | ((4) << 0)),

    SDL_PIXELFORMAT_ARGB2101010 =
        ((1 << 31) | ((SDL_PIXELTYPE_PACKED32) << 24) | ((SDL_PACKEDORDER_ARGB) << 20) | ((SDL_PACKEDLAYOUT_2101010) << 16) | ((32) << 8) | ((4) << 0)),


    SDL_PIXELFORMAT_YV12 =
        ((((Uint32)(((Uint8)(('Y'))))) << 0) | (((Uint32)(((Uint8)(('V'))))) << 8) | (((Uint32)(((Uint8)(('1'))))) << 16) | (((Uint32)(((Uint8)(('2'))))) << 24)),
    SDL_PIXELFORMAT_IYUV =
        ((((Uint32)(((Uint8)(('I'))))) << 0) | (((Uint32)(((Uint8)(('Y'))))) << 8) | (((Uint32)(((Uint8)(('U'))))) << 16) | (((Uint32)(((Uint8)(('V'))))) << 24)),
    SDL_PIXELFORMAT_YUY2 =
        ((((Uint32)(((Uint8)(('Y'))))) << 0) | (((Uint32)(((Uint8)(('U'))))) << 8) | (((Uint32)(((Uint8)(('Y'))))) << 16) | (((Uint32)(((Uint8)(('2'))))) << 24)),
    SDL_PIXELFORMAT_UYVY =
        ((((Uint32)(((Uint8)(('U'))))) << 0) | (((Uint32)(((Uint8)(('Y'))))) << 8) | (((Uint32)(((Uint8)(('V'))))) << 16) | (((Uint32)(((Uint8)(('Y'))))) << 24)),
    SDL_PIXELFORMAT_YVYU =
        ((((Uint32)(((Uint8)(('Y'))))) << 0) | (((Uint32)(((Uint8)(('V'))))) << 8) | (((Uint32)(((Uint8)(('Y'))))) << 16) | (((Uint32)(((Uint8)(('U'))))) << 24))
};

typedef struct SDL_Color
{
    Uint8 r;
    Uint8 g;
    Uint8 b;
    Uint8 unused;
} SDL_Color;


typedef struct SDL_Palette
{
    int ncolors;
    SDL_Color *colors;
    Uint32 version;
    int refcount;
} SDL_Palette;




typedef struct SDL_PixelFormat
{
    Uint32 format;
    SDL_Palette *palette;
    Uint8 BitsPerPixel;
    Uint8 BytesPerPixel;
    Uint8 padding[2];
    Uint32 Rmask;
    Uint32 Gmask;
    Uint32 Bmask;
    Uint32 Amask;
    Uint8 Rloss;
    Uint8 Gloss;
    Uint8 Bloss;
    Uint8 Aloss;
    Uint8 Rshift;
    Uint8 Gshift;
    Uint8 Bshift;
    Uint8 Ashift;
    int refcount;
    struct SDL_PixelFormat *next;
} SDL_PixelFormat;




extern __attribute__ ((visibility("default"))) const char* SDL_GetPixelFormatName(Uint32 format);
# 307 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_PixelFormatEnumToMasks(Uint32 format,
                                                            int *bpp,
                                                            Uint32 * Rmask,
                                                            Uint32 * Gmask,
                                                            Uint32 * Bmask,
                                                            Uint32 * Amask);
# 322 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
extern __attribute__ ((visibility("default"))) Uint32 SDL_MasksToPixelFormatEnum(int bpp,
                                                          Uint32 Rmask,
                                                          Uint32 Gmask,
                                                          Uint32 Bmask,
                                                          Uint32 Amask);




extern __attribute__ ((visibility("default"))) SDL_PixelFormat * SDL_AllocFormat(Uint32 pixel_format);




extern __attribute__ ((visibility("default"))) void SDL_FreeFormat(SDL_PixelFormat *format);
# 348 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
extern __attribute__ ((visibility("default"))) SDL_Palette * SDL_AllocPalette(int ncolors);




extern __attribute__ ((visibility("default"))) int SDL_SetPixelFormatPalette(SDL_PixelFormat * format,
                                                      SDL_Palette *palette);
# 366 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
extern __attribute__ ((visibility("default"))) int SDL_SetPaletteColors(SDL_Palette * palette,
                                                 const SDL_Color * colors,
                                                 int firstcolor, int ncolors);






extern __attribute__ ((visibility("default"))) void SDL_FreePalette(SDL_Palette * palette);






extern __attribute__ ((visibility("default"))) Uint32 SDL_MapRGB(const SDL_PixelFormat * format,
                                          Uint8 r, Uint8 g, Uint8 b);






extern __attribute__ ((visibility("default"))) Uint32 SDL_MapRGBA(const SDL_PixelFormat * format,
                                           Uint8 r, Uint8 g, Uint8 b,
                                           Uint8 a);






extern __attribute__ ((visibility("default"))) void SDL_GetRGB(Uint32 pixel,
                                        const SDL_PixelFormat * format,
                                        Uint8 * r, Uint8 * g, Uint8 * b);






extern __attribute__ ((visibility("default"))) void SDL_GetRGBA(Uint32 pixel,
                                         const SDL_PixelFormat * format,
                                         Uint8 * r, Uint8 * g, Uint8 * b,
                                         Uint8 * a);




extern __attribute__ ((visibility("default"))) void SDL_CalculateGammaRamp(float gamma, Uint16 * ramp);
# 425 "../Frameworks/SDL.framework/Headers/SDL_pixels.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 426 "../Frameworks/SDL.framework/Headers/SDL_pixels.h" 2
# 33 "../Frameworks/SDL.framework/Headers/SDL_video.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_rect.h" 1
# 36 "../Frameworks/SDL.framework/Headers/SDL_rect.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 37 "../Frameworks/SDL.framework/Headers/SDL_rect.h" 2
# 49 "../Frameworks/SDL.framework/Headers/SDL_rect.h"
typedef struct
{
    int x;
    int y;
} SDL_Point;
# 65 "../Frameworks/SDL.framework/Headers/SDL_rect.h"
typedef struct SDL_Rect
{
    int x, y;
    int w, h;
} SDL_Rect;
# 88 "../Frameworks/SDL.framework/Headers/SDL_rect.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasIntersection(const SDL_Rect * A,
                                                     const SDL_Rect * B);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_IntersectRect(const SDL_Rect * A,
                                                   const SDL_Rect * B,
                                                   SDL_Rect * result);




extern __attribute__ ((visibility("default"))) void SDL_UnionRect(const SDL_Rect * A,
                                           const SDL_Rect * B,
                                           SDL_Rect * result);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_EnclosePoints(const SDL_Point * points,
                                                   int count,
                                                   const SDL_Rect * clip,
                                                   SDL_Rect * result);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_IntersectRectAndLine(const SDL_Rect *
                                                          rect, int *X1,
                                                          int *Y1, int *X2,
                                                          int *Y2);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 134 "../Frameworks/SDL.framework/Headers/SDL_rect.h" 2
# 34 "../Frameworks/SDL.framework/Headers/SDL_video.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_surface.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_blendmode.h" 1
# 31 "../Frameworks/SDL.framework/Headers/SDL_blendmode.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 32 "../Frameworks/SDL.framework/Headers/SDL_blendmode.h" 2
# 42 "../Frameworks/SDL.framework/Headers/SDL_blendmode.h"
typedef enum
{
    SDL_BLENDMODE_NONE = 0x00000000,
    SDL_BLENDMODE_BLEND = 0x00000001,
    SDL_BLENDMODE_ADD = 0x00000002,
    SDL_BLENDMODE_MOD = 0x00000004
} SDL_BlendMode;







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 57 "../Frameworks/SDL.framework/Headers/SDL_blendmode.h" 2
# 35 "../Frameworks/SDL.framework/Headers/SDL_surface.h" 2


# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 38 "../Frameworks/SDL.framework/Headers/SDL_surface.h" 2
# 70 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
typedef struct SDL_Surface
{
    Uint32 flags;
    SDL_PixelFormat *format;
    int w, h;
    int pitch;
    void *pixels;


    void *userdata;


    int locked;
    void *lock_data;


    SDL_Rect clip_rect;


    struct SDL_BlitMap *map;


    int refcount;
} SDL_Surface;




typedef int (*SDL_blit) (struct SDL_Surface * src, SDL_Rect * srcrect,
                         struct SDL_Surface * dst, SDL_Rect * dstrect);
# 112 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_CreateRGBSurface
    (Uint32 flags, int width, int height, int depth,
     Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_CreateRGBSurfaceFrom(void *pixels,
                                                              int width,
                                                              int height,
                                                              int depth,
                                                              int pitch,
                                                              Uint32 Rmask,
                                                              Uint32 Gmask,
                                                              Uint32 Bmask,
                                                              Uint32 Amask);
extern __attribute__ ((visibility("default"))) void SDL_FreeSurface(SDL_Surface * surface);
# 133 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetSurfacePalette(SDL_Surface * surface,
                                                  SDL_Palette * palette);
# 155 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_LockSurface(SDL_Surface * surface);

extern __attribute__ ((visibility("default"))) void SDL_UnlockSurface(SDL_Surface * surface);
# 168 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_LoadBMP_RW(SDL_RWops * src,
                                                    int freesrc);
# 185 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SaveBMP_RW
    (SDL_Surface * surface, SDL_RWops * dst, int freedst);
# 204 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetSurfaceRLE(SDL_Surface * surface,
                                              int flag);
# 216 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetColorKey(SDL_Surface * surface,
                                            int flag, Uint32 key);
# 229 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_GetColorKey(SDL_Surface * surface,
                                            Uint32 * key);
# 244 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetSurfaceColorMod(SDL_Surface * surface,
                                                   Uint8 r, Uint8 g, Uint8 b);
# 260 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_GetSurfaceColorMod(SDL_Surface * surface,
                                                   Uint8 * r, Uint8 * g,
                                                   Uint8 * b);
# 274 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetSurfaceAlphaMod(SDL_Surface * surface,
                                                   Uint8 alpha);
# 287 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_GetSurfaceAlphaMod(SDL_Surface * surface,
                                                   Uint8 * alpha);
# 300 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_SetSurfaceBlendMode(SDL_Surface * surface,
                                                    SDL_BlendMode blendMode);
# 313 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_GetSurfaceBlendMode(SDL_Surface * surface,
                                                    SDL_BlendMode *blendMode);
# 329 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_SetClipRect(SDL_Surface * surface,
                                                 const SDL_Rect * rect);







extern __attribute__ ((visibility("default"))) void SDL_GetClipRect(SDL_Surface * surface,
                                             SDL_Rect * rect);
# 351 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_ConvertSurface
    (SDL_Surface * src, SDL_PixelFormat * fmt, Uint32 flags);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_ConvertSurfaceFormat
    (SDL_Surface * src, Uint32 pixel_format, Uint32 flags);






extern __attribute__ ((visibility("default"))) int SDL_ConvertPixels(int width, int height,
                                              Uint32 src_format,
                                              const void * src, int src_pitch,
                                              Uint32 dst_format,
                                              void * dst, int dst_pitch);
# 377 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_FillRect
    (SDL_Surface * dst, const SDL_Rect * rect, Uint32 color);
extern __attribute__ ((visibility("default"))) int SDL_FillRects
    (SDL_Surface * dst, const SDL_Rect * rects, int count, Uint32 color);
# 447 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
extern __attribute__ ((visibility("default"))) int SDL_UpperBlit
    (SDL_Surface * src, const SDL_Rect * srcrect,
     SDL_Surface * dst, SDL_Rect * dstrect);





extern __attribute__ ((visibility("default"))) int SDL_LowerBlit
    (SDL_Surface * src, SDL_Rect * srcrect,
     SDL_Surface * dst, SDL_Rect * dstrect);







extern __attribute__ ((visibility("default"))) int SDL_SoftStretch(SDL_Surface * src,
                                            const SDL_Rect * srcrect,
                                            SDL_Surface * dst,
                                            const SDL_Rect * dstrect);







extern __attribute__ ((visibility("default"))) int SDL_UpperBlitScaled
    (SDL_Surface * src, const SDL_Rect * srcrect,
    SDL_Surface * dst, SDL_Rect * dstrect);





extern __attribute__ ((visibility("default"))) int SDL_LowerBlitScaled
    (SDL_Surface * src, SDL_Rect * srcrect,
    SDL_Surface * dst, SDL_Rect * dstrect);
# 495 "../Frameworks/SDL.framework/Headers/SDL_surface.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 496 "../Frameworks/SDL.framework/Headers/SDL_surface.h" 2
# 35 "../Frameworks/SDL.framework/Headers/SDL_video.h" 2

# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 37 "../Frameworks/SDL.framework/Headers/SDL_video.h" 2
# 55 "../Frameworks/SDL.framework/Headers/SDL_video.h"
typedef struct
{
    Uint32 format;
    int w;
    int h;
    int refresh_rate;
    void *driverdata;
} SDL_DisplayMode;
# 90 "../Frameworks/SDL.framework/Headers/SDL_video.h"
typedef struct SDL_Window SDL_Window;






typedef enum
{
    SDL_WINDOW_FULLSCREEN = 0x00000001,
    SDL_WINDOW_OPENGL = 0x00000002,
    SDL_WINDOW_SHOWN = 0x00000004,
    SDL_WINDOW_HIDDEN = 0x00000008,
    SDL_WINDOW_BORDERLESS = 0x00000010,
    SDL_WINDOW_RESIZABLE = 0x00000020,
    SDL_WINDOW_MINIMIZED = 0x00000040,
    SDL_WINDOW_MAXIMIZED = 0x00000080,
    SDL_WINDOW_INPUT_GRABBED = 0x00000100,
    SDL_WINDOW_INPUT_FOCUS = 0x00000200,
    SDL_WINDOW_MOUSE_FOCUS = 0x00000400,
    SDL_WINDOW_FOREIGN = 0x00000800
} SDL_WindowFlags;
# 134 "../Frameworks/SDL.framework/Headers/SDL_video.h"
typedef enum
{
    SDL_WINDOWEVENT_NONE,
    SDL_WINDOWEVENT_SHOWN,
    SDL_WINDOWEVENT_HIDDEN,
    SDL_WINDOWEVENT_EXPOSED,

    SDL_WINDOWEVENT_MOVED,

    SDL_WINDOWEVENT_RESIZED,
    SDL_WINDOWEVENT_SIZE_CHANGED,
    SDL_WINDOWEVENT_MINIMIZED,
    SDL_WINDOWEVENT_MAXIMIZED,
    SDL_WINDOWEVENT_RESTORED,

    SDL_WINDOWEVENT_ENTER,
    SDL_WINDOWEVENT_LEAVE,
    SDL_WINDOWEVENT_FOCUS_GAINED,
    SDL_WINDOWEVENT_FOCUS_LOST,
    SDL_WINDOWEVENT_CLOSE

} SDL_WindowEventID;




typedef void *SDL_GLContext;




typedef enum
{
    SDL_GL_RED_SIZE,
    SDL_GL_GREEN_SIZE,
    SDL_GL_BLUE_SIZE,
    SDL_GL_ALPHA_SIZE,
    SDL_GL_BUFFER_SIZE,
    SDL_GL_DOUBLEBUFFER,
    SDL_GL_DEPTH_SIZE,
    SDL_GL_STENCIL_SIZE,
    SDL_GL_ACCUM_RED_SIZE,
    SDL_GL_ACCUM_GREEN_SIZE,
    SDL_GL_ACCUM_BLUE_SIZE,
    SDL_GL_ACCUM_ALPHA_SIZE,
    SDL_GL_STEREO,
    SDL_GL_MULTISAMPLEBUFFERS,
    SDL_GL_MULTISAMPLESAMPLES,
    SDL_GL_ACCELERATED_VISUAL,
    SDL_GL_RETAINED_BACKING,
    SDL_GL_CONTEXT_MAJOR_VERSION,
    SDL_GL_CONTEXT_MINOR_VERSION
} SDL_GLattr;
# 196 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GetNumVideoDrivers(void);
# 206 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetVideoDriver(int index);
# 222 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_VideoInit(const char *driver_name);
# 231 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void SDL_VideoQuit(void);
# 242 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetCurrentVideoDriver(void);






extern __attribute__ ((visibility("default"))) int SDL_GetNumVideoDisplays(void);
# 259 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GetDisplayBounds(int displayIndex, SDL_Rect * rect);






extern __attribute__ ((visibility("default"))) int SDL_GetNumDisplayModes(int displayIndex);
# 279 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GetDisplayMode(int displayIndex, int modeIndex,
                                               SDL_DisplayMode * mode);




extern __attribute__ ((visibility("default"))) int SDL_GetDesktopDisplayMode(int displayIndex, SDL_DisplayMode * mode);




extern __attribute__ ((visibility("default"))) int SDL_GetCurrentDisplayMode(int displayIndex, SDL_DisplayMode * mode);
# 313 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_DisplayMode * SDL_GetClosestDisplayMode(int displayIndex, const SDL_DisplayMode * mode, SDL_DisplayMode * closest);







extern __attribute__ ((visibility("default"))) int SDL_GetWindowDisplay(SDL_Window * window);
# 336 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_SetWindowDisplayMode(SDL_Window * window,
                                                     const SDL_DisplayMode
                                                         * mode);
# 347 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GetWindowDisplayMode(SDL_Window * window,
                                                     SDL_DisplayMode * mode);




extern __attribute__ ((visibility("default"))) Uint32 SDL_GetWindowPixelFormat(SDL_Window * window);
# 375 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_Window * SDL_CreateWindow(const char *title,
                                                      int x, int y, int w,
                                                      int h, Uint32 flags);
# 388 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_Window * SDL_CreateWindowFrom(const void *data);




extern __attribute__ ((visibility("default"))) Uint32 SDL_GetWindowID(SDL_Window * window);




extern __attribute__ ((visibility("default"))) SDL_Window * SDL_GetWindowFromID(Uint32 id);




extern __attribute__ ((visibility("default"))) Uint32 SDL_GetWindowFlags(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_SetWindowTitle(SDL_Window * window,
                                                const char *title);






extern __attribute__ ((visibility("default"))) const char * SDL_GetWindowTitle(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_SetWindowIcon(SDL_Window * window,
                                               SDL_Surface * icon);
# 441 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void* SDL_SetWindowData(SDL_Window * window,
                                                const char *name,
                                                void *userdata);
# 455 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void * SDL_GetWindowData(SDL_Window * window,
                                                const char *name);
# 471 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void SDL_SetWindowPosition(SDL_Window * window,
                                                   int x, int y);






extern __attribute__ ((visibility("default"))) void SDL_GetWindowPosition(SDL_Window * window,
                                                   int *x, int *y);
# 490 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void SDL_SetWindowSize(SDL_Window * window, int w,
                                               int h);






extern __attribute__ ((visibility("default"))) void SDL_GetWindowSize(SDL_Window * window, int *w,
                                               int *h);






extern __attribute__ ((visibility("default"))) void SDL_ShowWindow(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_HideWindow(SDL_Window * window);




extern __attribute__ ((visibility("default"))) void SDL_RaiseWindow(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_MaximizeWindow(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_MinimizeWindow(SDL_Window * window);







extern __attribute__ ((visibility("default"))) void SDL_RestoreWindow(SDL_Window * window);
# 550 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_SetWindowFullscreen(SDL_Window * window,
                                                    SDL_bool fullscreen);
# 566 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_GetWindowSurface(SDL_Window * window);
# 576 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_UpdateWindowSurface(SDL_Window * window);
# 586 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_UpdateWindowSurfaceRects(SDL_Window * window,
                                                         SDL_Rect * rects,
                                                         int numrects);
# 597 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) void SDL_SetWindowGrab(SDL_Window * window,
                                               SDL_bool grabbed);
# 607 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_GetWindowGrab(SDL_Window * window);
# 617 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_SetWindowBrightness(SDL_Window * window, float brightness);
# 626 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) float SDL_GetWindowBrightness(SDL_Window * window);
# 645 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_SetWindowGammaRamp(SDL_Window * window,
                                                   const Uint16 * red,
                                                   const Uint16 * green,
                                                   const Uint16 * blue);
# 664 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GetWindowGammaRamp(SDL_Window * window,
                                                   Uint16 * red,
                                                   Uint16 * green,
                                                   Uint16 * blue);




extern __attribute__ ((visibility("default"))) void SDL_DestroyWindow(SDL_Window * window);
# 681 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_IsScreenSaverEnabled(void);







extern __attribute__ ((visibility("default"))) void SDL_EnableScreenSaver(void);







extern __attribute__ ((visibility("default"))) void SDL_DisableScreenSaver(void);
# 723 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GL_LoadLibrary(const char *path);




extern __attribute__ ((visibility("default"))) void * SDL_GL_GetProcAddress(const char *proc);






extern __attribute__ ((visibility("default"))) void SDL_GL_UnloadLibrary(void);





extern __attribute__ ((visibility("default"))) SDL_bool SDL_GL_ExtensionSupported(const char
                                                           *extension);




extern __attribute__ ((visibility("default"))) int SDL_GL_SetAttribute(SDL_GLattr attr, int value);




extern __attribute__ ((visibility("default"))) int SDL_GL_GetAttribute(SDL_GLattr attr, int *value);







extern __attribute__ ((visibility("default"))) SDL_GLContext SDL_GL_CreateContext(SDL_Window *
                                                           window);






extern __attribute__ ((visibility("default"))) int SDL_GL_MakeCurrent(SDL_Window * window,
                                               SDL_GLContext context);
# 781 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GL_SetSwapInterval(int interval);
# 792 "../Frameworks/SDL.framework/Headers/SDL_video.h"
extern __attribute__ ((visibility("default"))) int SDL_GL_GetSwapInterval(void);





extern __attribute__ ((visibility("default"))) void SDL_GL_SwapWindow(SDL_Window * window);






extern __attribute__ ((visibility("default"))) void SDL_GL_DeleteContext(SDL_GLContext context);
# 816 "../Frameworks/SDL.framework/Headers/SDL_video.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 817 "../Frameworks/SDL.framework/Headers/SDL_video.h" 2
# 34 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_keycode.h" 1
# 32 "../Frameworks/SDL.framework/Headers/SDL_keycode.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_scancode.h" 1
# 43 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
typedef enum
{
    SDL_SCANCODE_UNKNOWN = 0,
# 54 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_A = 4,
    SDL_SCANCODE_B = 5,
    SDL_SCANCODE_C = 6,
    SDL_SCANCODE_D = 7,
    SDL_SCANCODE_E = 8,
    SDL_SCANCODE_F = 9,
    SDL_SCANCODE_G = 10,
    SDL_SCANCODE_H = 11,
    SDL_SCANCODE_I = 12,
    SDL_SCANCODE_J = 13,
    SDL_SCANCODE_K = 14,
    SDL_SCANCODE_L = 15,
    SDL_SCANCODE_M = 16,
    SDL_SCANCODE_N = 17,
    SDL_SCANCODE_O = 18,
    SDL_SCANCODE_P = 19,
    SDL_SCANCODE_Q = 20,
    SDL_SCANCODE_R = 21,
    SDL_SCANCODE_S = 22,
    SDL_SCANCODE_T = 23,
    SDL_SCANCODE_U = 24,
    SDL_SCANCODE_V = 25,
    SDL_SCANCODE_W = 26,
    SDL_SCANCODE_X = 27,
    SDL_SCANCODE_Y = 28,
    SDL_SCANCODE_Z = 29,

    SDL_SCANCODE_1 = 30,
    SDL_SCANCODE_2 = 31,
    SDL_SCANCODE_3 = 32,
    SDL_SCANCODE_4 = 33,
    SDL_SCANCODE_5 = 34,
    SDL_SCANCODE_6 = 35,
    SDL_SCANCODE_7 = 36,
    SDL_SCANCODE_8 = 37,
    SDL_SCANCODE_9 = 38,
    SDL_SCANCODE_0 = 39,

    SDL_SCANCODE_RETURN = 40,
    SDL_SCANCODE_ESCAPE = 41,
    SDL_SCANCODE_BACKSPACE = 42,
    SDL_SCANCODE_TAB = 43,
    SDL_SCANCODE_SPACE = 44,

    SDL_SCANCODE_MINUS = 45,
    SDL_SCANCODE_EQUALS = 46,
    SDL_SCANCODE_LEFTBRACKET = 47,
    SDL_SCANCODE_RIGHTBRACKET = 48,
    SDL_SCANCODE_BACKSLASH = 49,
# 116 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_NONUSHASH = 50,
# 128 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_SEMICOLON = 51,
    SDL_SCANCODE_APOSTROPHE = 52,
    SDL_SCANCODE_GRAVE = 53,
# 147 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_COMMA = 54,
    SDL_SCANCODE_PERIOD = 55,
    SDL_SCANCODE_SLASH = 56,

    SDL_SCANCODE_CAPSLOCK = 57,

    SDL_SCANCODE_F1 = 58,
    SDL_SCANCODE_F2 = 59,
    SDL_SCANCODE_F3 = 60,
    SDL_SCANCODE_F4 = 61,
    SDL_SCANCODE_F5 = 62,
    SDL_SCANCODE_F6 = 63,
    SDL_SCANCODE_F7 = 64,
    SDL_SCANCODE_F8 = 65,
    SDL_SCANCODE_F9 = 66,
    SDL_SCANCODE_F10 = 67,
    SDL_SCANCODE_F11 = 68,
    SDL_SCANCODE_F12 = 69,

    SDL_SCANCODE_PRINTSCREEN = 70,
    SDL_SCANCODE_SCROLLLOCK = 71,
    SDL_SCANCODE_PAUSE = 72,
    SDL_SCANCODE_INSERT = 73,

    SDL_SCANCODE_HOME = 74,
    SDL_SCANCODE_PAGEUP = 75,
    SDL_SCANCODE_DELETE = 76,
    SDL_SCANCODE_END = 77,
    SDL_SCANCODE_PAGEDOWN = 78,
    SDL_SCANCODE_RIGHT = 79,
    SDL_SCANCODE_LEFT = 80,
    SDL_SCANCODE_DOWN = 81,
    SDL_SCANCODE_UP = 82,

    SDL_SCANCODE_NUMLOCKCLEAR = 83,

    SDL_SCANCODE_KP_DIVIDE = 84,
    SDL_SCANCODE_KP_MULTIPLY = 85,
    SDL_SCANCODE_KP_MINUS = 86,
    SDL_SCANCODE_KP_PLUS = 87,
    SDL_SCANCODE_KP_ENTER = 88,
    SDL_SCANCODE_KP_1 = 89,
    SDL_SCANCODE_KP_2 = 90,
    SDL_SCANCODE_KP_3 = 91,
    SDL_SCANCODE_KP_4 = 92,
    SDL_SCANCODE_KP_5 = 93,
    SDL_SCANCODE_KP_6 = 94,
    SDL_SCANCODE_KP_7 = 95,
    SDL_SCANCODE_KP_8 = 96,
    SDL_SCANCODE_KP_9 = 97,
    SDL_SCANCODE_KP_0 = 98,
    SDL_SCANCODE_KP_PERIOD = 99,

    SDL_SCANCODE_NONUSBACKSLASH = 100,
# 210 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_APPLICATION = 101,
    SDL_SCANCODE_POWER = 102,


    SDL_SCANCODE_KP_EQUALS = 103,
    SDL_SCANCODE_F13 = 104,
    SDL_SCANCODE_F14 = 105,
    SDL_SCANCODE_F15 = 106,
    SDL_SCANCODE_F16 = 107,
    SDL_SCANCODE_F17 = 108,
    SDL_SCANCODE_F18 = 109,
    SDL_SCANCODE_F19 = 110,
    SDL_SCANCODE_F20 = 111,
    SDL_SCANCODE_F21 = 112,
    SDL_SCANCODE_F22 = 113,
    SDL_SCANCODE_F23 = 114,
    SDL_SCANCODE_F24 = 115,
    SDL_SCANCODE_EXECUTE = 116,
    SDL_SCANCODE_HELP = 117,
    SDL_SCANCODE_MENU = 118,
    SDL_SCANCODE_SELECT = 119,
    SDL_SCANCODE_STOP = 120,
    SDL_SCANCODE_AGAIN = 121,
    SDL_SCANCODE_UNDO = 122,
    SDL_SCANCODE_CUT = 123,
    SDL_SCANCODE_COPY = 124,
    SDL_SCANCODE_PASTE = 125,
    SDL_SCANCODE_FIND = 126,
    SDL_SCANCODE_MUTE = 127,
    SDL_SCANCODE_VOLUMEUP = 128,
    SDL_SCANCODE_VOLUMEDOWN = 129,




    SDL_SCANCODE_KP_COMMA = 133,
    SDL_SCANCODE_KP_EQUALSAS400 = 134,

    SDL_SCANCODE_INTERNATIONAL1 = 135,

    SDL_SCANCODE_INTERNATIONAL2 = 136,
    SDL_SCANCODE_INTERNATIONAL3 = 137,
    SDL_SCANCODE_INTERNATIONAL4 = 138,
    SDL_SCANCODE_INTERNATIONAL5 = 139,
    SDL_SCANCODE_INTERNATIONAL6 = 140,
    SDL_SCANCODE_INTERNATIONAL7 = 141,
    SDL_SCANCODE_INTERNATIONAL8 = 142,
    SDL_SCANCODE_INTERNATIONAL9 = 143,
    SDL_SCANCODE_LANG1 = 144,
    SDL_SCANCODE_LANG2 = 145,
    SDL_SCANCODE_LANG3 = 146,
    SDL_SCANCODE_LANG4 = 147,
    SDL_SCANCODE_LANG5 = 148,
    SDL_SCANCODE_LANG6 = 149,
    SDL_SCANCODE_LANG7 = 150,
    SDL_SCANCODE_LANG8 = 151,
    SDL_SCANCODE_LANG9 = 152,

    SDL_SCANCODE_ALTERASE = 153,
    SDL_SCANCODE_SYSREQ = 154,
    SDL_SCANCODE_CANCEL = 155,
    SDL_SCANCODE_CLEAR = 156,
    SDL_SCANCODE_PRIOR = 157,
    SDL_SCANCODE_RETURN2 = 158,
    SDL_SCANCODE_SEPARATOR = 159,
    SDL_SCANCODE_OUT = 160,
    SDL_SCANCODE_OPER = 161,
    SDL_SCANCODE_CLEARAGAIN = 162,
    SDL_SCANCODE_CRSEL = 163,
    SDL_SCANCODE_EXSEL = 164,

    SDL_SCANCODE_KP_00 = 176,
    SDL_SCANCODE_KP_000 = 177,
    SDL_SCANCODE_THOUSANDSSEPARATOR = 178,
    SDL_SCANCODE_DECIMALSEPARATOR = 179,
    SDL_SCANCODE_CURRENCYUNIT = 180,
    SDL_SCANCODE_CURRENCYSUBUNIT = 181,
    SDL_SCANCODE_KP_LEFTPAREN = 182,
    SDL_SCANCODE_KP_RIGHTPAREN = 183,
    SDL_SCANCODE_KP_LEFTBRACE = 184,
    SDL_SCANCODE_KP_RIGHTBRACE = 185,
    SDL_SCANCODE_KP_TAB = 186,
    SDL_SCANCODE_KP_BACKSPACE = 187,
    SDL_SCANCODE_KP_A = 188,
    SDL_SCANCODE_KP_B = 189,
    SDL_SCANCODE_KP_C = 190,
    SDL_SCANCODE_KP_D = 191,
    SDL_SCANCODE_KP_E = 192,
    SDL_SCANCODE_KP_F = 193,
    SDL_SCANCODE_KP_XOR = 194,
    SDL_SCANCODE_KP_POWER = 195,
    SDL_SCANCODE_KP_PERCENT = 196,
    SDL_SCANCODE_KP_LESS = 197,
    SDL_SCANCODE_KP_GREATER = 198,
    SDL_SCANCODE_KP_AMPERSAND = 199,
    SDL_SCANCODE_KP_DBLAMPERSAND = 200,
    SDL_SCANCODE_KP_VERTICALBAR = 201,
    SDL_SCANCODE_KP_DBLVERTICALBAR = 202,
    SDL_SCANCODE_KP_COLON = 203,
    SDL_SCANCODE_KP_HASH = 204,
    SDL_SCANCODE_KP_SPACE = 205,
    SDL_SCANCODE_KP_AT = 206,
    SDL_SCANCODE_KP_EXCLAM = 207,
    SDL_SCANCODE_KP_MEMSTORE = 208,
    SDL_SCANCODE_KP_MEMRECALL = 209,
    SDL_SCANCODE_KP_MEMCLEAR = 210,
    SDL_SCANCODE_KP_MEMADD = 211,
    SDL_SCANCODE_KP_MEMSUBTRACT = 212,
    SDL_SCANCODE_KP_MEMMULTIPLY = 213,
    SDL_SCANCODE_KP_MEMDIVIDE = 214,
    SDL_SCANCODE_KP_PLUSMINUS = 215,
    SDL_SCANCODE_KP_CLEAR = 216,
    SDL_SCANCODE_KP_CLEARENTRY = 217,
    SDL_SCANCODE_KP_BINARY = 218,
    SDL_SCANCODE_KP_OCTAL = 219,
    SDL_SCANCODE_KP_DECIMAL = 220,
    SDL_SCANCODE_KP_HEXADECIMAL = 221,

    SDL_SCANCODE_LCTRL = 224,
    SDL_SCANCODE_LSHIFT = 225,
    SDL_SCANCODE_LALT = 226,
    SDL_SCANCODE_LGUI = 227,
    SDL_SCANCODE_RCTRL = 228,
    SDL_SCANCODE_RSHIFT = 229,
    SDL_SCANCODE_RALT = 230,
    SDL_SCANCODE_RGUI = 231,

    SDL_SCANCODE_MODE = 257,
# 351 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_AUDIONEXT = 258,
    SDL_SCANCODE_AUDIOPREV = 259,
    SDL_SCANCODE_AUDIOSTOP = 260,
    SDL_SCANCODE_AUDIOPLAY = 261,
    SDL_SCANCODE_AUDIOMUTE = 262,
    SDL_SCANCODE_MEDIASELECT = 263,
    SDL_SCANCODE_WWW = 264,
    SDL_SCANCODE_MAIL = 265,
    SDL_SCANCODE_CALCULATOR = 266,
    SDL_SCANCODE_COMPUTER = 267,
    SDL_SCANCODE_AC_SEARCH = 268,
    SDL_SCANCODE_AC_HOME = 269,
    SDL_SCANCODE_AC_BACK = 270,
    SDL_SCANCODE_AC_FORWARD = 271,
    SDL_SCANCODE_AC_STOP = 272,
    SDL_SCANCODE_AC_REFRESH = 273,
    SDL_SCANCODE_AC_BOOKMARKS = 274,
# 378 "../Frameworks/SDL.framework/Headers/SDL_scancode.h"
    SDL_SCANCODE_BRIGHTNESSDOWN = 275,
    SDL_SCANCODE_BRIGHTNESSUP = 276,
    SDL_SCANCODE_DISPLAYSWITCH = 277,

    SDL_SCANCODE_KBDILLUMTOGGLE = 278,
    SDL_SCANCODE_KBDILLUMDOWN = 279,
    SDL_SCANCODE_KBDILLUMUP = 280,
    SDL_SCANCODE_EJECT = 281,
    SDL_SCANCODE_SLEEP = 282,





    SDL_NUM_SCANCODES = 512

} SDL_Scancode;
# 33 "../Frameworks/SDL.framework/Headers/SDL_keycode.h" 2
# 42 "../Frameworks/SDL.framework/Headers/SDL_keycode.h"
typedef Sint32 SDL_Keycode;




enum
{
    SDLK_UNKNOWN = 0,

    SDLK_RETURN = '\r',
    SDLK_ESCAPE = '\033',
    SDLK_BACKSPACE = '\b',
    SDLK_TAB = '\t',
    SDLK_SPACE = ' ',
    SDLK_EXCLAIM = '!',
    SDLK_QUOTEDBL = '"',
    SDLK_HASH = '#',
    SDLK_PERCENT = '%',
    SDLK_DOLLAR = '$',
    SDLK_AMPERSAND = '&',
    SDLK_QUOTE = '\'',
    SDLK_LEFTPAREN = '(',
    SDLK_RIGHTPAREN = ')',
    SDLK_ASTERISK = '*',
    SDLK_PLUS = '+',
    SDLK_COMMA = ',',
    SDLK_MINUS = '-',
    SDLK_PERIOD = '.',
    SDLK_SLASH = '/',
    SDLK_0 = '0',
    SDLK_1 = '1',
    SDLK_2 = '2',
    SDLK_3 = '3',
    SDLK_4 = '4',
    SDLK_5 = '5',
    SDLK_6 = '6',
    SDLK_7 = '7',
    SDLK_8 = '8',
    SDLK_9 = '9',
    SDLK_COLON = ':',
    SDLK_SEMICOLON = ';',
    SDLK_LESS = '<',
    SDLK_EQUALS = '=',
    SDLK_GREATER = '>',
    SDLK_QUESTION = '?',
    SDLK_AT = '@',



    SDLK_LEFTBRACKET = '[',
    SDLK_BACKSLASH = '\\',
    SDLK_RIGHTBRACKET = ']',
    SDLK_CARET = '^',
    SDLK_UNDERSCORE = '_',
    SDLK_BACKQUOTE = '`',
    SDLK_a = 'a',
    SDLK_b = 'b',
    SDLK_c = 'c',
    SDLK_d = 'd',
    SDLK_e = 'e',
    SDLK_f = 'f',
    SDLK_g = 'g',
    SDLK_h = 'h',
    SDLK_i = 'i',
    SDLK_j = 'j',
    SDLK_k = 'k',
    SDLK_l = 'l',
    SDLK_m = 'm',
    SDLK_n = 'n',
    SDLK_o = 'o',
    SDLK_p = 'p',
    SDLK_q = 'q',
    SDLK_r = 'r',
    SDLK_s = 's',
    SDLK_t = 't',
    SDLK_u = 'u',
    SDLK_v = 'v',
    SDLK_w = 'w',
    SDLK_x = 'x',
    SDLK_y = 'y',
    SDLK_z = 'z',

    SDLK_CAPSLOCK = (SDL_SCANCODE_CAPSLOCK | (1<<30)),

    SDLK_F1 = (SDL_SCANCODE_F1 | (1<<30)),
    SDLK_F2 = (SDL_SCANCODE_F2 | (1<<30)),
    SDLK_F3 = (SDL_SCANCODE_F3 | (1<<30)),
    SDLK_F4 = (SDL_SCANCODE_F4 | (1<<30)),
    SDLK_F5 = (SDL_SCANCODE_F5 | (1<<30)),
    SDLK_F6 = (SDL_SCANCODE_F6 | (1<<30)),
    SDLK_F7 = (SDL_SCANCODE_F7 | (1<<30)),
    SDLK_F8 = (SDL_SCANCODE_F8 | (1<<30)),
    SDLK_F9 = (SDL_SCANCODE_F9 | (1<<30)),
    SDLK_F10 = (SDL_SCANCODE_F10 | (1<<30)),
    SDLK_F11 = (SDL_SCANCODE_F11 | (1<<30)),
    SDLK_F12 = (SDL_SCANCODE_F12 | (1<<30)),

    SDLK_PRINTSCREEN = (SDL_SCANCODE_PRINTSCREEN | (1<<30)),
    SDLK_SCROLLLOCK = (SDL_SCANCODE_SCROLLLOCK | (1<<30)),
    SDLK_PAUSE = (SDL_SCANCODE_PAUSE | (1<<30)),
    SDLK_INSERT = (SDL_SCANCODE_INSERT | (1<<30)),
    SDLK_HOME = (SDL_SCANCODE_HOME | (1<<30)),
    SDLK_PAGEUP = (SDL_SCANCODE_PAGEUP | (1<<30)),
    SDLK_DELETE = '\177',
    SDLK_END = (SDL_SCANCODE_END | (1<<30)),
    SDLK_PAGEDOWN = (SDL_SCANCODE_PAGEDOWN | (1<<30)),
    SDLK_RIGHT = (SDL_SCANCODE_RIGHT | (1<<30)),
    SDLK_LEFT = (SDL_SCANCODE_LEFT | (1<<30)),
    SDLK_DOWN = (SDL_SCANCODE_DOWN | (1<<30)),
    SDLK_UP = (SDL_SCANCODE_UP | (1<<30)),

    SDLK_NUMLOCKCLEAR = (SDL_SCANCODE_NUMLOCKCLEAR | (1<<30)),
    SDLK_KP_DIVIDE = (SDL_SCANCODE_KP_DIVIDE | (1<<30)),
    SDLK_KP_MULTIPLY = (SDL_SCANCODE_KP_MULTIPLY | (1<<30)),
    SDLK_KP_MINUS = (SDL_SCANCODE_KP_MINUS | (1<<30)),
    SDLK_KP_PLUS = (SDL_SCANCODE_KP_PLUS | (1<<30)),
    SDLK_KP_ENTER = (SDL_SCANCODE_KP_ENTER | (1<<30)),
    SDLK_KP_1 = (SDL_SCANCODE_KP_1 | (1<<30)),
    SDLK_KP_2 = (SDL_SCANCODE_KP_2 | (1<<30)),
    SDLK_KP_3 = (SDL_SCANCODE_KP_3 | (1<<30)),
    SDLK_KP_4 = (SDL_SCANCODE_KP_4 | (1<<30)),
    SDLK_KP_5 = (SDL_SCANCODE_KP_5 | (1<<30)),
    SDLK_KP_6 = (SDL_SCANCODE_KP_6 | (1<<30)),
    SDLK_KP_7 = (SDL_SCANCODE_KP_7 | (1<<30)),
    SDLK_KP_8 = (SDL_SCANCODE_KP_8 | (1<<30)),
    SDLK_KP_9 = (SDL_SCANCODE_KP_9 | (1<<30)),
    SDLK_KP_0 = (SDL_SCANCODE_KP_0 | (1<<30)),
    SDLK_KP_PERIOD = (SDL_SCANCODE_KP_PERIOD | (1<<30)),

    SDLK_APPLICATION = (SDL_SCANCODE_APPLICATION | (1<<30)),
    SDLK_POWER = (SDL_SCANCODE_POWER | (1<<30)),
    SDLK_KP_EQUALS = (SDL_SCANCODE_KP_EQUALS | (1<<30)),
    SDLK_F13 = (SDL_SCANCODE_F13 | (1<<30)),
    SDLK_F14 = (SDL_SCANCODE_F14 | (1<<30)),
    SDLK_F15 = (SDL_SCANCODE_F15 | (1<<30)),
    SDLK_F16 = (SDL_SCANCODE_F16 | (1<<30)),
    SDLK_F17 = (SDL_SCANCODE_F17 | (1<<30)),
    SDLK_F18 = (SDL_SCANCODE_F18 | (1<<30)),
    SDLK_F19 = (SDL_SCANCODE_F19 | (1<<30)),
    SDLK_F20 = (SDL_SCANCODE_F20 | (1<<30)),
    SDLK_F21 = (SDL_SCANCODE_F21 | (1<<30)),
    SDLK_F22 = (SDL_SCANCODE_F22 | (1<<30)),
    SDLK_F23 = (SDL_SCANCODE_F23 | (1<<30)),
    SDLK_F24 = (SDL_SCANCODE_F24 | (1<<30)),
    SDLK_EXECUTE = (SDL_SCANCODE_EXECUTE | (1<<30)),
    SDLK_HELP = (SDL_SCANCODE_HELP | (1<<30)),
    SDLK_MENU = (SDL_SCANCODE_MENU | (1<<30)),
    SDLK_SELECT = (SDL_SCANCODE_SELECT | (1<<30)),
    SDLK_STOP = (SDL_SCANCODE_STOP | (1<<30)),
    SDLK_AGAIN = (SDL_SCANCODE_AGAIN | (1<<30)),
    SDLK_UNDO = (SDL_SCANCODE_UNDO | (1<<30)),
    SDLK_CUT = (SDL_SCANCODE_CUT | (1<<30)),
    SDLK_COPY = (SDL_SCANCODE_COPY | (1<<30)),
    SDLK_PASTE = (SDL_SCANCODE_PASTE | (1<<30)),
    SDLK_FIND = (SDL_SCANCODE_FIND | (1<<30)),
    SDLK_MUTE = (SDL_SCANCODE_MUTE | (1<<30)),
    SDLK_VOLUMEUP = (SDL_SCANCODE_VOLUMEUP | (1<<30)),
    SDLK_VOLUMEDOWN = (SDL_SCANCODE_VOLUMEDOWN | (1<<30)),
    SDLK_KP_COMMA = (SDL_SCANCODE_KP_COMMA | (1<<30)),
    SDLK_KP_EQUALSAS400 =
        (SDL_SCANCODE_KP_EQUALSAS400 | (1<<30)),

    SDLK_ALTERASE = (SDL_SCANCODE_ALTERASE | (1<<30)),
    SDLK_SYSREQ = (SDL_SCANCODE_SYSREQ | (1<<30)),
    SDLK_CANCEL = (SDL_SCANCODE_CANCEL | (1<<30)),
    SDLK_CLEAR = (SDL_SCANCODE_CLEAR | (1<<30)),
    SDLK_PRIOR = (SDL_SCANCODE_PRIOR | (1<<30)),
    SDLK_RETURN2 = (SDL_SCANCODE_RETURN2 | (1<<30)),
    SDLK_SEPARATOR = (SDL_SCANCODE_SEPARATOR | (1<<30)),
    SDLK_OUT = (SDL_SCANCODE_OUT | (1<<30)),
    SDLK_OPER = (SDL_SCANCODE_OPER | (1<<30)),
    SDLK_CLEARAGAIN = (SDL_SCANCODE_CLEARAGAIN | (1<<30)),
    SDLK_CRSEL = (SDL_SCANCODE_CRSEL | (1<<30)),
    SDLK_EXSEL = (SDL_SCANCODE_EXSEL | (1<<30)),

    SDLK_KP_00 = (SDL_SCANCODE_KP_00 | (1<<30)),
    SDLK_KP_000 = (SDL_SCANCODE_KP_000 | (1<<30)),
    SDLK_THOUSANDSSEPARATOR =
        (SDL_SCANCODE_THOUSANDSSEPARATOR | (1<<30)),
    SDLK_DECIMALSEPARATOR =
        (SDL_SCANCODE_DECIMALSEPARATOR | (1<<30)),
    SDLK_CURRENCYUNIT = (SDL_SCANCODE_CURRENCYUNIT | (1<<30)),
    SDLK_CURRENCYSUBUNIT =
        (SDL_SCANCODE_CURRENCYSUBUNIT | (1<<30)),
    SDLK_KP_LEFTPAREN = (SDL_SCANCODE_KP_LEFTPAREN | (1<<30)),
    SDLK_KP_RIGHTPAREN = (SDL_SCANCODE_KP_RIGHTPAREN | (1<<30)),
    SDLK_KP_LEFTBRACE = (SDL_SCANCODE_KP_LEFTBRACE | (1<<30)),
    SDLK_KP_RIGHTBRACE = (SDL_SCANCODE_KP_RIGHTBRACE | (1<<30)),
    SDLK_KP_TAB = (SDL_SCANCODE_KP_TAB | (1<<30)),
    SDLK_KP_BACKSPACE = (SDL_SCANCODE_KP_BACKSPACE | (1<<30)),
    SDLK_KP_A = (SDL_SCANCODE_KP_A | (1<<30)),
    SDLK_KP_B = (SDL_SCANCODE_KP_B | (1<<30)),
    SDLK_KP_C = (SDL_SCANCODE_KP_C | (1<<30)),
    SDLK_KP_D = (SDL_SCANCODE_KP_D | (1<<30)),
    SDLK_KP_E = (SDL_SCANCODE_KP_E | (1<<30)),
    SDLK_KP_F = (SDL_SCANCODE_KP_F | (1<<30)),
    SDLK_KP_XOR = (SDL_SCANCODE_KP_XOR | (1<<30)),
    SDLK_KP_POWER = (SDL_SCANCODE_KP_POWER | (1<<30)),
    SDLK_KP_PERCENT = (SDL_SCANCODE_KP_PERCENT | (1<<30)),
    SDLK_KP_LESS = (SDL_SCANCODE_KP_LESS | (1<<30)),
    SDLK_KP_GREATER = (SDL_SCANCODE_KP_GREATER | (1<<30)),
    SDLK_KP_AMPERSAND = (SDL_SCANCODE_KP_AMPERSAND | (1<<30)),
    SDLK_KP_DBLAMPERSAND =
        (SDL_SCANCODE_KP_DBLAMPERSAND | (1<<30)),
    SDLK_KP_VERTICALBAR =
        (SDL_SCANCODE_KP_VERTICALBAR | (1<<30)),
    SDLK_KP_DBLVERTICALBAR =
        (SDL_SCANCODE_KP_DBLVERTICALBAR | (1<<30)),
    SDLK_KP_COLON = (SDL_SCANCODE_KP_COLON | (1<<30)),
    SDLK_KP_HASH = (SDL_SCANCODE_KP_HASH | (1<<30)),
    SDLK_KP_SPACE = (SDL_SCANCODE_KP_SPACE | (1<<30)),
    SDLK_KP_AT = (SDL_SCANCODE_KP_AT | (1<<30)),
    SDLK_KP_EXCLAM = (SDL_SCANCODE_KP_EXCLAM | (1<<30)),
    SDLK_KP_MEMSTORE = (SDL_SCANCODE_KP_MEMSTORE | (1<<30)),
    SDLK_KP_MEMRECALL = (SDL_SCANCODE_KP_MEMRECALL | (1<<30)),
    SDLK_KP_MEMCLEAR = (SDL_SCANCODE_KP_MEMCLEAR | (1<<30)),
    SDLK_KP_MEMADD = (SDL_SCANCODE_KP_MEMADD | (1<<30)),
    SDLK_KP_MEMSUBTRACT =
        (SDL_SCANCODE_KP_MEMSUBTRACT | (1<<30)),
    SDLK_KP_MEMMULTIPLY =
        (SDL_SCANCODE_KP_MEMMULTIPLY | (1<<30)),
    SDLK_KP_MEMDIVIDE = (SDL_SCANCODE_KP_MEMDIVIDE | (1<<30)),
    SDLK_KP_PLUSMINUS = (SDL_SCANCODE_KP_PLUSMINUS | (1<<30)),
    SDLK_KP_CLEAR = (SDL_SCANCODE_KP_CLEAR | (1<<30)),
    SDLK_KP_CLEARENTRY = (SDL_SCANCODE_KP_CLEARENTRY | (1<<30)),
    SDLK_KP_BINARY = (SDL_SCANCODE_KP_BINARY | (1<<30)),
    SDLK_KP_OCTAL = (SDL_SCANCODE_KP_OCTAL | (1<<30)),
    SDLK_KP_DECIMAL = (SDL_SCANCODE_KP_DECIMAL | (1<<30)),
    SDLK_KP_HEXADECIMAL =
        (SDL_SCANCODE_KP_HEXADECIMAL | (1<<30)),

    SDLK_LCTRL = (SDL_SCANCODE_LCTRL | (1<<30)),
    SDLK_LSHIFT = (SDL_SCANCODE_LSHIFT | (1<<30)),
    SDLK_LALT = (SDL_SCANCODE_LALT | (1<<30)),
    SDLK_LGUI = (SDL_SCANCODE_LGUI | (1<<30)),
    SDLK_RCTRL = (SDL_SCANCODE_RCTRL | (1<<30)),
    SDLK_RSHIFT = (SDL_SCANCODE_RSHIFT | (1<<30)),
    SDLK_RALT = (SDL_SCANCODE_RALT | (1<<30)),
    SDLK_RGUI = (SDL_SCANCODE_RGUI | (1<<30)),

    SDLK_MODE = (SDL_SCANCODE_MODE | (1<<30)),

    SDLK_AUDIONEXT = (SDL_SCANCODE_AUDIONEXT | (1<<30)),
    SDLK_AUDIOPREV = (SDL_SCANCODE_AUDIOPREV | (1<<30)),
    SDLK_AUDIOSTOP = (SDL_SCANCODE_AUDIOSTOP | (1<<30)),
    SDLK_AUDIOPLAY = (SDL_SCANCODE_AUDIOPLAY | (1<<30)),
    SDLK_AUDIOMUTE = (SDL_SCANCODE_AUDIOMUTE | (1<<30)),
    SDLK_MEDIASELECT = (SDL_SCANCODE_MEDIASELECT | (1<<30)),
    SDLK_WWW = (SDL_SCANCODE_WWW | (1<<30)),
    SDLK_MAIL = (SDL_SCANCODE_MAIL | (1<<30)),
    SDLK_CALCULATOR = (SDL_SCANCODE_CALCULATOR | (1<<30)),
    SDLK_COMPUTER = (SDL_SCANCODE_COMPUTER | (1<<30)),
    SDLK_AC_SEARCH = (SDL_SCANCODE_AC_SEARCH | (1<<30)),
    SDLK_AC_HOME = (SDL_SCANCODE_AC_HOME | (1<<30)),
    SDLK_AC_BACK = (SDL_SCANCODE_AC_BACK | (1<<30)),
    SDLK_AC_FORWARD = (SDL_SCANCODE_AC_FORWARD | (1<<30)),
    SDLK_AC_STOP = (SDL_SCANCODE_AC_STOP | (1<<30)),
    SDLK_AC_REFRESH = (SDL_SCANCODE_AC_REFRESH | (1<<30)),
    SDLK_AC_BOOKMARKS = (SDL_SCANCODE_AC_BOOKMARKS | (1<<30)),

    SDLK_BRIGHTNESSDOWN =
        (SDL_SCANCODE_BRIGHTNESSDOWN | (1<<30)),
    SDLK_BRIGHTNESSUP = (SDL_SCANCODE_BRIGHTNESSUP | (1<<30)),
    SDLK_DISPLAYSWITCH = (SDL_SCANCODE_DISPLAYSWITCH | (1<<30)),
    SDLK_KBDILLUMTOGGLE =
        (SDL_SCANCODE_KBDILLUMTOGGLE | (1<<30)),
    SDLK_KBDILLUMDOWN = (SDL_SCANCODE_KBDILLUMDOWN | (1<<30)),
    SDLK_KBDILLUMUP = (SDL_SCANCODE_KBDILLUMUP | (1<<30)),
    SDLK_EJECT = (SDL_SCANCODE_EJECT | (1<<30)),
    SDLK_SLEEP = (SDL_SCANCODE_SLEEP | (1<<30))
};




typedef enum
{
    KMOD_NONE = 0x0000,
    KMOD_LSHIFT = 0x0001,
    KMOD_RSHIFT = 0x0002,
    KMOD_LCTRL = 0x0040,
    KMOD_RCTRL = 0x0080,
    KMOD_LALT = 0x0100,
    KMOD_RALT = 0x0200,
    KMOD_LGUI = 0x0400,
    KMOD_RGUI = 0x0800,
    KMOD_NUM = 0x1000,
    KMOD_CAPS = 0x2000,
    KMOD_MODE = 0x4000,
    KMOD_RESERVED = 0x8000
} SDL_Keymod;
# 34 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h" 2


# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 37 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h" 2
# 47 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
typedef struct SDL_Keysym
{
    SDL_Scancode scancode;
    SDL_Keycode sym;
    Uint16 mod;
    Uint32 unicode;
} SDL_Keysym;






extern __attribute__ ((visibility("default"))) SDL_Window * SDL_GetKeyboardFocus(void);
# 77 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) Uint8 * SDL_GetKeyboardState(int *numkeys);




extern __attribute__ ((visibility("default"))) SDL_Keymod SDL_GetModState(void);






extern __attribute__ ((visibility("default"))) void SDL_SetModState(SDL_Keymod modstate);
# 99 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) SDL_Keycode SDL_GetKeyFromScancode(SDL_Scancode scancode);
# 109 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) SDL_Scancode SDL_GetScancodeFromKey(SDL_Keycode key);
# 120 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetScancodeName(SDL_Scancode scancode);
# 129 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) SDL_Scancode SDL_GetScancodeFromName(const char *name);
# 141 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetKeyName(SDL_Keycode key);
# 150 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
extern __attribute__ ((visibility("default"))) SDL_Keycode SDL_GetKeyFromName(const char *name);







extern __attribute__ ((visibility("default"))) void SDL_StartTextInput(void);






extern __attribute__ ((visibility("default"))) void SDL_StopTextInput(void);






extern __attribute__ ((visibility("default"))) void SDL_SetTextInputRect(SDL_Rect *rect);
# 181 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 182 "../Frameworks/SDL.framework/Headers/SDL_keyboard.h" 2
# 35 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_mouse.h" 1
# 51 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 52 "../Frameworks/SDL.framework/Headers/SDL_mouse.h" 2







typedef struct SDL_Cursor SDL_Cursor;







extern __attribute__ ((visibility("default"))) SDL_Window * SDL_GetMouseFocus(void);
# 77 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) Uint8 SDL_GetMouseState(int *x, int *y);
# 86 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) Uint8 SDL_GetRelativeMouseState(int *x, int *y);
# 97 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) void SDL_WarpMouseInWindow(SDL_Window * window,
                                                   int x, int y);
# 116 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) int SDL_SetRelativeMouseMode(SDL_bool enabled);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_GetRelativeMouseMode(void);
# 143 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) SDL_Cursor * SDL_CreateCursor(const Uint8 * data,
                                                     const Uint8 * mask,
                                                     int w, int h, int hot_x,
                                                     int hot_y);






extern __attribute__ ((visibility("default"))) SDL_Cursor * SDL_CreateColorCursor(SDL_Surface *surface,
                                                          int hot_x,
                                                          int hot_y);




extern __attribute__ ((visibility("default"))) void SDL_SetCursor(SDL_Cursor * cursor);




extern __attribute__ ((visibility("default"))) SDL_Cursor * SDL_GetCursor(void);






extern __attribute__ ((visibility("default"))) void SDL_FreeCursor(SDL_Cursor * cursor);
# 182 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
extern __attribute__ ((visibility("default"))) int SDL_ShowCursor(int toggle);
# 209 "../Frameworks/SDL.framework/Headers/SDL_mouse.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 210 "../Frameworks/SDL.framework/Headers/SDL_mouse.h" 2
# 36 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_joystick.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_joystick.h" 2
# 51 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
struct _SDL_Joystick;
typedef struct _SDL_Joystick SDL_Joystick;






extern __attribute__ ((visibility("default"))) int SDL_NumJoysticks(void);






extern __attribute__ ((visibility("default"))) const char * SDL_JoystickName(int device_index);
# 76 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
extern __attribute__ ((visibility("default"))) SDL_Joystick * SDL_JoystickOpen(int device_index);




extern __attribute__ ((visibility("default"))) int SDL_JoystickOpened(int device_index);




extern __attribute__ ((visibility("default"))) int SDL_JoystickIndex(SDL_Joystick * joystick);




extern __attribute__ ((visibility("default"))) int SDL_JoystickNumAxes(SDL_Joystick * joystick);







extern __attribute__ ((visibility("default"))) int SDL_JoystickNumBalls(SDL_Joystick * joystick);




extern __attribute__ ((visibility("default"))) int SDL_JoystickNumHats(SDL_Joystick * joystick);




extern __attribute__ ((visibility("default"))) int SDL_JoystickNumButtons(SDL_Joystick * joystick);







extern __attribute__ ((visibility("default"))) void SDL_JoystickUpdate(void);
# 128 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
extern __attribute__ ((visibility("default"))) int SDL_JoystickEventState(int state);
# 137 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
extern __attribute__ ((visibility("default"))) Sint16 SDL_JoystickGetAxis(SDL_Joystick * joystick,
                                                   int axis);
# 171 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
extern __attribute__ ((visibility("default"))) Uint8 SDL_JoystickGetHat(SDL_Joystick * joystick,
                                                 int hat);
# 181 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
extern __attribute__ ((visibility("default"))) int SDL_JoystickGetBall(SDL_Joystick * joystick,
                                                int ball, int *dx, int *dy);






extern __attribute__ ((visibility("default"))) Uint8 SDL_JoystickGetButton(SDL_Joystick * joystick,
                                                    int button);




extern __attribute__ ((visibility("default"))) void SDL_JoystickClose(SDL_Joystick * joystick);
# 204 "../Frameworks/SDL.framework/Headers/SDL_joystick.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 205 "../Frameworks/SDL.framework/Headers/SDL_joystick.h" 2
# 37 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_quit.h" 1
# 38 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_gesture.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_gesture.h"
# 1 "../Frameworks/SDL.framework/Headers/SDL_touch.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_touch.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 36 "../Frameworks/SDL.framework/Headers/SDL_touch.h" 2
# 44 "../Frameworks/SDL.framework/Headers/SDL_touch.h"
typedef Sint64 SDL_TouchID;
typedef Sint64 SDL_FingerID;


struct SDL_Finger {
  SDL_FingerID id;
  Uint16 x;
  Uint16 y;
  Uint16 pressure;
  Uint16 xdelta;
  Uint16 ydelta;
  Uint16 last_x, last_y,last_pressure;
  SDL_bool down;
};

typedef struct SDL_Touch SDL_Touch;
typedef struct SDL_Finger SDL_Finger;


struct SDL_Touch {


  void (*FreeTouch) (SDL_Touch * touch);


  float pressure_max, pressure_min;
  float x_max,x_min;
  float y_max,y_min;
  Uint16 xres,yres,pressureres;
  float native_xres,native_yres,native_pressureres;
  float tilt;
  float rotation;


  SDL_TouchID id;
  SDL_Window *focus;

  char *name;
  Uint8 buttonstate;
  SDL_bool relative_mode;
  SDL_bool flush_motion;

  int num_fingers;
  int max_fingers;
  SDL_Finger** fingers;

  void *driverdata;
};
# 102 "../Frameworks/SDL.framework/Headers/SDL_touch.h"
  extern __attribute__ ((visibility("default"))) SDL_Touch* SDL_GetTouch(SDL_TouchID id);
# 111 "../Frameworks/SDL.framework/Headers/SDL_touch.h"
  extern
  __attribute__ ((visibility("default"))) SDL_Finger* SDL_GetFinger(SDL_Touch *touch, SDL_FingerID id);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 121 "../Frameworks/SDL.framework/Headers/SDL_touch.h" 2
# 36 "../Frameworks/SDL.framework/Headers/SDL_gesture.h" 2


# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 39 "../Frameworks/SDL.framework/Headers/SDL_gesture.h" 2







typedef Sint64 SDL_GestureID;
# 55 "../Frameworks/SDL.framework/Headers/SDL_gesture.h"
extern __attribute__ ((visibility("default"))) int SDL_RecordGesture(SDL_TouchID touchId);







extern __attribute__ ((visibility("default"))) int SDL_SaveAllDollarTemplates(SDL_RWops *src);






extern __attribute__ ((visibility("default"))) int SDL_SaveDollarTemplate(SDL_GestureID gestureId,SDL_RWops *src);







extern __attribute__ ((visibility("default"))) int SDL_LoadDollarTemplates(SDL_TouchID touchId, SDL_RWops *src);
# 87 "../Frameworks/SDL.framework/Headers/SDL_gesture.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 88 "../Frameworks/SDL.framework/Headers/SDL_gesture.h" 2
# 39 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2


# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 42 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 56 "../Frameworks/SDL.framework/Headers/SDL_events.h"
typedef enum
{
    SDL_FIRSTEVENT = 0,


    SDL_QUIT = 0x100,


    SDL_WINDOWEVENT = 0x200,
    SDL_SYSWMEVENT,


    SDL_KEYDOWN = 0x300,
    SDL_KEYUP,
    SDL_TEXTEDITING,
    SDL_TEXTINPUT,


    SDL_MOUSEMOTION = 0x400,
    SDL_MOUSEBUTTONDOWN,
    SDL_MOUSEBUTTONUP,
    SDL_MOUSEWHEEL,


    SDL_INPUTMOTION = 0x500,
    SDL_INPUTBUTTONDOWN,
    SDL_INPUTBUTTONUP,
    SDL_INPUTWHEEL,
    SDL_INPUTPROXIMITYIN,
    SDL_INPUTPROXIMITYOUT,


    SDL_JOYAXISMOTION = 0x600,
    SDL_JOYBALLMOTION,
    SDL_JOYHATMOTION,
    SDL_JOYBUTTONDOWN,
    SDL_JOYBUTTONUP,


    SDL_FINGERDOWN = 0x700,
    SDL_FINGERUP,
    SDL_FINGERMOTION,
    SDL_TOUCHBUTTONDOWN,
    SDL_TOUCHBUTTONUP,


    SDL_DOLLARGESTURE = 0x800,
    SDL_DOLLARRECORD,
    SDL_MULTIGESTURE,


    SDL_CLIPBOARDUPDATE = 0x900,


    SDL_DROPFILE = 0x1000,


    SDL_EVENT_COMPAT1 = 0x7000,
    SDL_EVENT_COMPAT2,
    SDL_EVENT_COMPAT3,





    SDL_USEREVENT = 0x8000,




    SDL_LASTEVENT = 0xFFFF
} SDL_EventType;




typedef struct SDL_WindowEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint8 event;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    int data1;
    int data2;
} SDL_WindowEvent;




typedef struct SDL_KeyboardEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint8 state;
    Uint8 repeat;
    Uint8 padding2;
    Uint8 padding3;
    SDL_Keysym keysym;
} SDL_KeyboardEvent;





typedef struct SDL_TextEditingEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    char text[(32)];
    int start;
    int length;
} SDL_TextEditingEvent;






typedef struct SDL_TextInputEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    char text[(32)];
} SDL_TextInputEvent;




typedef struct SDL_MouseMotionEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    int x;
    int y;
    int xrel;
    int yrel;
} SDL_MouseMotionEvent;




typedef struct SDL_MouseButtonEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    Uint8 button;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
    int x;
    int y;
} SDL_MouseButtonEvent;




typedef struct SDL_MouseWheelEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    int x;
    int y;
} SDL_MouseWheelEvent;




typedef struct SDL_JoyAxisEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint8 which;
    Uint8 axis;
    Uint8 padding1;
    Uint8 padding2;
    int value;
} SDL_JoyAxisEvent;




typedef struct SDL_JoyBallEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint8 which;
    Uint8 ball;
    Uint8 padding1;
    Uint8 padding2;
    int xrel;
    int yrel;
} SDL_JoyBallEvent;




typedef struct SDL_JoyHatEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint8 which;
    Uint8 hat;
    Uint8 value;






    Uint8 padding1;
} SDL_JoyHatEvent;




typedef struct SDL_JoyButtonEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint8 which;
    Uint8 button;
    Uint8 state;
    Uint8 padding1;
} SDL_JoyButtonEvent;





typedef struct SDL_TouchFingerEvent
{
    Uint32 type;

    Uint32 timestamp;
    Uint32 windowID;
    SDL_TouchID touchId;
    SDL_FingerID fingerId;
    Uint8 state;
    Uint8 padding1;
    Uint8 padding2;
    Uint8 padding3;
    Uint16 x;
    Uint16 y;
    Sint16 dx;
    Sint16 dy;
    Uint16 pressure;
} SDL_TouchFingerEvent;





typedef struct SDL_TouchButtonEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    SDL_TouchID touchId;
    Uint8 state;
    Uint8 button;
    Uint8 padding1;
    Uint8 padding2;
} SDL_TouchButtonEvent;





typedef struct SDL_MultiGestureEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    SDL_TouchID touchId;
    float dTheta;
    float dDist;
    float x;
    float y;
    Uint16 numFingers;
    Uint16 padding;
} SDL_MultiGestureEvent;


typedef struct SDL_DollarGestureEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    SDL_TouchID touchId;
    SDL_GestureID gestureId;
    Uint32 numFingers;
    float error;





} SDL_DollarGestureEvent;







typedef struct SDL_DropEvent
{
    Uint32 type;
    Uint32 timestamp;
    char *file;
} SDL_DropEvent;





typedef struct SDL_QuitEvent
{
    Uint32 type;
    Uint32 timestamp;
} SDL_QuitEvent;





typedef struct SDL_UserEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;
    int code;
    void *data1;
    void *data2;
} SDL_UserEvent;


struct SDL_SysWMmsg;
typedef struct SDL_SysWMmsg SDL_SysWMmsg;







typedef struct SDL_SysWMEvent
{
    Uint32 type;
    Uint32 timestamp;
    SDL_SysWMmsg *msg;
} SDL_SysWMEvent;
# 432 "../Frameworks/SDL.framework/Headers/SDL_events.h"
typedef struct SDL_ActiveEvent
{
    Uint32 type;
    Uint32 timestamp;
    Uint8 gain;
    Uint8 state;
} SDL_ActiveEvent;

typedef struct SDL_ResizeEvent
{
    Uint32 type;
    Uint32 timestamp;
    int w;
    int h;
} SDL_ResizeEvent;
# 455 "../Frameworks/SDL.framework/Headers/SDL_events.h"
typedef union SDL_Event
{
    Uint32 type;
    SDL_WindowEvent window;
    SDL_KeyboardEvent key;
    SDL_TextEditingEvent edit;
    SDL_TextInputEvent text;
    SDL_MouseMotionEvent motion;
    SDL_MouseButtonEvent button;
    SDL_MouseWheelEvent wheel;
    SDL_JoyAxisEvent jaxis;
    SDL_JoyBallEvent jball;
    SDL_JoyHatEvent jhat;
    SDL_JoyButtonEvent jbutton;
    SDL_QuitEvent quit;
    SDL_UserEvent user;
    SDL_SysWMEvent syswm;
    SDL_TouchFingerEvent tfinger;
    SDL_TouchButtonEvent tbutton;
    SDL_MultiGestureEvent mgesture;
    SDL_DollarGestureEvent dgesture;
    SDL_DropEvent drop;




    SDL_ActiveEvent active;
    SDL_ResizeEvent resize;


} SDL_Event;
# 497 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) void SDL_PumpEvents(void);


typedef enum
{
    SDL_ADDEVENT,
    SDL_PEEKEVENT,
    SDL_GETEVENT
} SDL_eventaction;
# 525 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) int SDL_PeepEvents(SDL_Event * events, int numevents,
                                           SDL_eventaction action,
                                           Uint32 minType, Uint32 maxType);





extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasEvent(Uint32 type);
extern __attribute__ ((visibility("default"))) SDL_bool SDL_HasEvents(Uint32 minType, Uint32 maxType);




extern __attribute__ ((visibility("default"))) void SDL_FlushEvent(Uint32 type);
extern __attribute__ ((visibility("default"))) void SDL_FlushEvents(Uint32 minType, Uint32 maxType);
# 550 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) int SDL_PollEvent(SDL_Event * event);
# 560 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) int SDL_WaitEvent(SDL_Event * event);
# 571 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) int SDL_WaitEventTimeout(SDL_Event * event,
                                                 int timeout);







extern __attribute__ ((visibility("default"))) int SDL_PushEvent(SDL_Event * event);

typedef int ( * SDL_EventFilter) (void *userdata, SDL_Event * event);
# 609 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) void SDL_SetEventFilter(SDL_EventFilter filter,
                                                void *userdata);





extern __attribute__ ((visibility("default"))) SDL_bool SDL_GetEventFilter(SDL_EventFilter * filter,
                                                    void **userdata);




extern __attribute__ ((visibility("default"))) void SDL_AddEventWatch(SDL_EventFilter filter,
                                               void *userdata);




extern __attribute__ ((visibility("default"))) void SDL_DelEventWatch(SDL_EventFilter filter,
                                               void *userdata);





extern __attribute__ ((visibility("default"))) void SDL_FilterEvents(SDL_EventFilter filter,
                                              void *userdata);
# 653 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) Uint8 SDL_EventState(Uint32 type, int state);
# 664 "../Frameworks/SDL.framework/Headers/SDL_events.h"
extern __attribute__ ((visibility("default"))) Uint32 SDL_RegisterEvents(int numevents);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 673 "../Frameworks/SDL.framework/Headers/SDL_events.h" 2
# 82 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_hints.h" 1
# 44 "../Frameworks/SDL.framework/Headers/SDL_hints.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 45 "../Frameworks/SDL.framework/Headers/SDL_hints.h" 2
# 151 "../Frameworks/SDL.framework/Headers/SDL_hints.h"
typedef enum
{
    SDL_HINT_DEFAULT,
    SDL_HINT_NORMAL,
    SDL_HINT_OVERRIDE
} SDL_HintPriority;
# 168 "../Frameworks/SDL.framework/Headers/SDL_hints.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_SetHintWithPriority(const char *name,
                                                         const char *value,
                                                         SDL_HintPriority priority);






extern __attribute__ ((visibility("default"))) SDL_bool SDL_SetHint(const char *name,
                                             const char *value);







extern __attribute__ ((visibility("default"))) const char * SDL_GetHint(const char *name);






extern __attribute__ ((visibility("default"))) void SDL_ClearHints(void);
# 202 "../Frameworks/SDL.framework/Headers/SDL_hints.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 203 "../Frameworks/SDL.framework/Headers/SDL_hints.h" 2
# 83 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_loadso.h" 1
# 47 "../Frameworks/SDL.framework/Headers/SDL_loadso.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 48 "../Frameworks/SDL.framework/Headers/SDL_loadso.h" 2
# 60 "../Frameworks/SDL.framework/Headers/SDL_loadso.h"
extern __attribute__ ((visibility("default"))) void * SDL_LoadObject(const char *sofile);






extern __attribute__ ((visibility("default"))) void * SDL_LoadFunction(void *handle,
                                               const char *name);




extern __attribute__ ((visibility("default"))) void SDL_UnloadObject(void *handle);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 82 "../Frameworks/SDL.framework/Headers/SDL_loadso.h" 2
# 84 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_log.h" 1
# 42 "../Frameworks/SDL.framework/Headers/SDL_log.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 43 "../Frameworks/SDL.framework/Headers/SDL_log.h" 2
# 64 "../Frameworks/SDL.framework/Headers/SDL_log.h"
enum
{
    SDL_LOG_CATEGORY_APPLICATION,
    SDL_LOG_CATEGORY_ERROR,
    SDL_LOG_CATEGORY_SYSTEM,
    SDL_LOG_CATEGORY_AUDIO,
    SDL_LOG_CATEGORY_VIDEO,
    SDL_LOG_CATEGORY_RENDER,
    SDL_LOG_CATEGORY_INPUT,


    SDL_LOG_CATEGORY_RESERVED1,
    SDL_LOG_CATEGORY_RESERVED2,
    SDL_LOG_CATEGORY_RESERVED3,
    SDL_LOG_CATEGORY_RESERVED4,
    SDL_LOG_CATEGORY_RESERVED5,
    SDL_LOG_CATEGORY_RESERVED6,
    SDL_LOG_CATEGORY_RESERVED7,
    SDL_LOG_CATEGORY_RESERVED8,
    SDL_LOG_CATEGORY_RESERVED9,
    SDL_LOG_CATEGORY_RESERVED10,
# 94 "../Frameworks/SDL.framework/Headers/SDL_log.h"
    SDL_LOG_CATEGORY_CUSTOM
};




typedef enum
{
    SDL_LOG_PRIORITY_VERBOSE = 1,
    SDL_LOG_PRIORITY_DEBUG,
    SDL_LOG_PRIORITY_INFO,
    SDL_LOG_PRIORITY_WARN,
    SDL_LOG_PRIORITY_ERROR,
    SDL_LOG_PRIORITY_CRITICAL,
    SDL_NUM_LOG_PRIORITIES
} SDL_LogPriority;





extern __attribute__ ((visibility("default"))) void SDL_LogSetAllPriority(SDL_LogPriority priority);




extern __attribute__ ((visibility("default"))) void SDL_LogSetPriority(int category,
                                                SDL_LogPriority priority);




extern __attribute__ ((visibility("default"))) SDL_LogPriority SDL_LogGetPriority(int category);






extern __attribute__ ((visibility("default"))) void SDL_LogResetPriorities(void);




extern __attribute__ ((visibility("default"))) void SDL_Log(const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogVerbose(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogDebug(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogInfo(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogWarn(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogError(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogCritical(int category, const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogMessage(int category,
                                            SDL_LogPriority priority,
                                            const char *fmt, ...);




extern __attribute__ ((visibility("default"))) void SDL_LogMessageV(int category,
                                             SDL_LogPriority priority,
                                             const char *fmt, va_list ap);




typedef void (*SDL_LogOutputFunction)(void *userdata, int category, SDL_LogPriority priority, const char *message);




extern __attribute__ ((visibility("default"))) void SDL_LogGetOutputFunction(SDL_LogOutputFunction *callback, void **userdata);





extern __attribute__ ((visibility("default"))) void SDL_LogSetOutputFunction(SDL_LogOutputFunction callback, void *userdata);
# 207 "../Frameworks/SDL.framework/Headers/SDL_log.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 208 "../Frameworks/SDL.framework/Headers/SDL_log.h" 2
# 85 "../Frameworks/SDL.framework/Headers/SDL.h" 2

# 1 "../Frameworks/SDL.framework/Headers/SDL_power.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_power.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_power.h" 2
# 44 "../Frameworks/SDL.framework/Headers/SDL_power.h"
typedef enum
{
    SDL_POWERSTATE_UNKNOWN,
    SDL_POWERSTATE_ON_BATTERY,
    SDL_POWERSTATE_NO_BATTERY,
    SDL_POWERSTATE_CHARGING,
    SDL_POWERSTATE_CHARGED
} SDL_PowerState;
# 67 "../Frameworks/SDL.framework/Headers/SDL_power.h"
extern __attribute__ ((visibility("default"))) SDL_PowerState SDL_GetPowerInfo(int *secs, int *pct);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 76 "../Frameworks/SDL.framework/Headers/SDL_power.h" 2
# 87 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_render.h" 1
# 52 "../Frameworks/SDL.framework/Headers/SDL_render.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 53 "../Frameworks/SDL.framework/Headers/SDL_render.h" 2
# 63 "../Frameworks/SDL.framework/Headers/SDL_render.h"
typedef enum
{
    SDL_RENDERER_SOFTWARE = 0x00000001,
    SDL_RENDERER_ACCELERATED = 0x00000002,

    SDL_RENDERER_PRESENTVSYNC = 0x00000004

} SDL_RendererFlags;




typedef struct SDL_RendererInfo
{
    const char *name;
    Uint32 flags;
    Uint32 num_texture_formats;
    Uint32 texture_formats[16];
    int max_texture_width;
    int max_texture_height;
} SDL_RendererInfo;




typedef enum
{
    SDL_TEXTUREACCESS_STATIC,
    SDL_TEXTUREACCESS_STREAMING
} SDL_TextureAccess;




typedef enum
{
    SDL_TEXTUREMODULATE_NONE = 0x00000000,
    SDL_TEXTUREMODULATE_COLOR = 0x00000001,
    SDL_TEXTUREMODULATE_ALPHA = 0x00000002
} SDL_TextureModulate;




struct SDL_Renderer;
typedef struct SDL_Renderer SDL_Renderer;




struct SDL_Texture;
typedef struct SDL_Texture SDL_Texture;
# 130 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetNumRenderDrivers(void);
# 144 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetRenderDriverInfo(int index,
                                                    SDL_RendererInfo * info);
# 161 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) SDL_Renderer * SDL_CreateRenderer(SDL_Window * window,
                                               int index, Uint32 flags);
# 174 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) SDL_Renderer * SDL_CreateSoftwareRenderer(SDL_Surface * surface);




extern __attribute__ ((visibility("default"))) SDL_Renderer * SDL_GetRenderer(SDL_Window * window);




extern __attribute__ ((visibility("default"))) int SDL_GetRendererInfo(SDL_Renderer * renderer,
                                                SDL_RendererInfo * info);
# 203 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) SDL_Texture * SDL_CreateTexture(SDL_Renderer * renderer,
                                                        Uint32 format,
                                                        int access, int w,
                                                        int h);
# 220 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) SDL_Texture * SDL_CreateTextureFromSurface(SDL_Renderer * renderer, SDL_Surface * surface);
# 235 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_QueryTexture(SDL_Texture * texture,
                                             Uint32 * format, int *access,
                                             int *w, int *h);
# 252 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_SetTextureColorMod(SDL_Texture * texture,
                                                   Uint8 r, Uint8 g, Uint8 b);
# 268 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetTextureColorMod(SDL_Texture * texture,
                                                   Uint8 * r, Uint8 * g,
                                                   Uint8 * b);
# 283 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_SetTextureAlphaMod(SDL_Texture * texture,
                                                   Uint8 alpha);
# 296 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetTextureAlphaMod(SDL_Texture * texture,
                                                   Uint8 * alpha);
# 313 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_SetTextureBlendMode(SDL_Texture * texture,
                                                    SDL_BlendMode blendMode);
# 326 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetTextureBlendMode(SDL_Texture * texture,
                                                    SDL_BlendMode *blendMode);
# 342 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_UpdateTexture(SDL_Texture * texture,
                                              const SDL_Rect * rect,
                                              const void *pixels, int pitch);
# 361 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_LockTexture(SDL_Texture * texture,
                                            const SDL_Rect * rect,
                                            void **pixels, int *pitch);






extern __attribute__ ((visibility("default"))) void SDL_UnlockTexture(SDL_Texture * texture);
# 382 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderSetViewport(SDL_Renderer * renderer,
                                                  const SDL_Rect * rect);




extern __attribute__ ((visibility("default"))) void SDL_RenderGetViewport(SDL_Renderer * renderer,
                                                   SDL_Rect * rect);
# 402 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_SetRenderDrawColor(SDL_Renderer * renderer,
                                           Uint8 r, Uint8 g, Uint8 b,
                                           Uint8 a);
# 417 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetRenderDrawColor(SDL_Renderer * renderer,
                                           Uint8 * r, Uint8 * g, Uint8 * b,
                                           Uint8 * a);
# 433 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_SetRenderDrawBlendMode(SDL_Renderer * renderer,
                                                       SDL_BlendMode blendMode);
# 445 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_GetRenderDrawBlendMode(SDL_Renderer * renderer,
                                                       SDL_BlendMode *blendMode);






extern __attribute__ ((visibility("default"))) int SDL_RenderClear(SDL_Renderer * renderer);
# 463 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawPoint(SDL_Renderer * renderer,
                                                int x, int y);
# 474 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawPoints(SDL_Renderer * renderer,
                                                 const SDL_Point * points,
                                                 int count);
# 488 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawLine(SDL_Renderer * renderer,
                                               int x1, int y1, int x2, int y2);
# 499 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawLines(SDL_Renderer * renderer,
                                                const SDL_Point * points,
                                                int count);
# 510 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawRect(SDL_Renderer * renderer,
                                               const SDL_Rect * rect);
# 521 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderDrawRects(SDL_Renderer * renderer,
                                                const SDL_Rect * rects,
                                                int count);
# 533 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderFillRect(SDL_Renderer * renderer,
                                               const SDL_Rect * rect);
# 544 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderFillRects(SDL_Renderer * renderer,
                                                const SDL_Rect * rects,
                                                int count);
# 559 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderCopy(SDL_Renderer * renderer,
                                           SDL_Texture * texture,
                                           const SDL_Rect * srcrect,
                                           const SDL_Rect * dstrect);
# 578 "../Frameworks/SDL.framework/Headers/SDL_render.h"
extern __attribute__ ((visibility("default"))) int SDL_RenderReadPixels(SDL_Renderer * renderer,
                                                 const SDL_Rect * rect,
                                                 Uint32 format,
                                                 void *pixels, int pitch);




extern __attribute__ ((visibility("default"))) void SDL_RenderPresent(SDL_Renderer * renderer);







extern __attribute__ ((visibility("default"))) void SDL_DestroyTexture(SDL_Texture * texture);







extern __attribute__ ((visibility("default"))) void SDL_DestroyRenderer(SDL_Renderer * renderer);
# 611 "../Frameworks/SDL.framework/Headers/SDL_render.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 612 "../Frameworks/SDL.framework/Headers/SDL_render.h" 2
# 88 "../Frameworks/SDL.framework/Headers/SDL.h" 2


# 1 "../Frameworks/SDL.framework/Headers/SDL_timer.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_timer.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 35 "../Frameworks/SDL.framework/Headers/SDL_timer.h" 2
# 47 "../Frameworks/SDL.framework/Headers/SDL_timer.h"
extern __attribute__ ((visibility("default"))) Uint32 SDL_GetTicks(void);




extern __attribute__ ((visibility("default"))) Uint64 SDL_GetPerformanceCounter(void);




extern __attribute__ ((visibility("default"))) Uint64 SDL_GetPerformanceFrequency(void);




extern __attribute__ ((visibility("default"))) void SDL_Delay(Uint32 ms);
# 72 "../Frameworks/SDL.framework/Headers/SDL_timer.h"
typedef Uint32 ( * SDL_TimerCallback) (Uint32 interval, void *param);




typedef int SDL_TimerID;






extern __attribute__ ((visibility("default"))) SDL_TimerID SDL_AddTimer(Uint32 interval,
                                                 SDL_TimerCallback callback,
                                                 void *param);
# 95 "../Frameworks/SDL.framework/Headers/SDL_timer.h"
extern __attribute__ ((visibility("default"))) SDL_bool SDL_RemoveTimer(SDL_TimerID id);
# 104 "../Frameworks/SDL.framework/Headers/SDL_timer.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 105 "../Frameworks/SDL.framework/Headers/SDL_timer.h" 2
# 91 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "../Frameworks/SDL.framework/Headers/SDL_version.h" 1
# 33 "../Frameworks/SDL.framework/Headers/SDL_version.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 34 "../Frameworks/SDL.framework/Headers/SDL_version.h" 2
# 53 "../Frameworks/SDL.framework/Headers/SDL_version.h"
typedef struct SDL_version
{
    Uint8 major;
    Uint8 minor;
    Uint8 patch;
} SDL_version;
# 135 "../Frameworks/SDL.framework/Headers/SDL_version.h"
extern __attribute__ ((visibility("default"))) void SDL_GetVersion(SDL_version * ver);
# 144 "../Frameworks/SDL.framework/Headers/SDL_version.h"
extern __attribute__ ((visibility("default"))) const char * SDL_GetRevision(void);
# 153 "../Frameworks/SDL.framework/Headers/SDL_version.h"
extern __attribute__ ((visibility("default"))) int SDL_GetRevisionNumber(void);
# 162 "../Frameworks/SDL.framework/Headers/SDL_version.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 163 "../Frameworks/SDL.framework/Headers/SDL_version.h" 2
# 92 "../Frameworks/SDL.framework/Headers/SDL.h" 2

# 1 "../Frameworks/SDL.framework/Headers/SDL_compat.h" 1
# 53 "../Frameworks/SDL.framework/Headers/SDL_compat.h"
# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 54 "../Frameworks/SDL.framework/Headers/SDL_compat.h" 2
# 134 "../Frameworks/SDL.framework/Headers/SDL_compat.h"
typedef struct SDL_VideoInfo
{
    Uint32 hw_available:1;
    Uint32 wm_available:1;
    Uint32 UnusedBits1:6;
    Uint32 UnusedBits2:1;
    Uint32 blit_hw:1;
    Uint32 blit_hw_CC:1;
    Uint32 blit_hw_A:1;
    Uint32 blit_sw:1;
    Uint32 blit_sw_CC:1;
    Uint32 blit_sw_A:1;
    Uint32 blit_fill:1;
    Uint32 UnusedBits3:16;
    Uint32 video_mem;

    SDL_PixelFormat *vfmt;

    int current_w;
    int current_h;
} SDL_VideoInfo;
# 178 "../Frameworks/SDL.framework/Headers/SDL_compat.h"
typedef struct SDL_Overlay
{
    Uint32 format;
    int w, h;
    int planes;
    Uint16 *pitches;
    Uint8 **pixels;





    struct private_yuvhwfuncs *hwfuncs;
    struct private_yuvhwdata *hwdata;






    Uint32 hw_overlay:1;
    Uint32 UnusedBits:31;

} SDL_Overlay;

typedef enum
{
    SDL_GRAB_QUERY = -1,
    SDL_GRAB_OFF = 0,
    SDL_GRAB_ON = 1
} SDL_GrabMode;

struct SDL_SysWMinfo;
# 278 "../Frameworks/SDL.framework/Headers/SDL_compat.h"
extern __attribute__ ((visibility("default"))) const SDL_version * SDL_Linked_Version(void);
extern __attribute__ ((visibility("default"))) const char * SDL_AudioDriverName(char *namebuf, int maxlen);
extern __attribute__ ((visibility("default"))) const char * SDL_VideoDriverName(char *namebuf, int maxlen);
extern __attribute__ ((visibility("default"))) const SDL_VideoInfo * SDL_GetVideoInfo(void);
extern __attribute__ ((visibility("default"))) int SDL_VideoModeOK(int width,
                                            int height,
                                            int bpp, Uint32 flags);
extern __attribute__ ((visibility("default"))) SDL_Rect ** SDL_ListModes(const SDL_PixelFormat *
                                                 format, Uint32 flags);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_SetVideoMode(int width, int height,
                                                      int bpp, Uint32 flags);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_GetVideoSurface(void);
extern __attribute__ ((visibility("default"))) void SDL_UpdateRects(SDL_Surface * screen,
                                             int numrects, SDL_Rect * rects);
extern __attribute__ ((visibility("default"))) void SDL_UpdateRect(SDL_Surface * screen,
                                            Sint32 x,
                                            Sint32 y, Uint32 w, Uint32 h);
extern __attribute__ ((visibility("default"))) int SDL_Flip(SDL_Surface * screen);
extern __attribute__ ((visibility("default"))) int SDL_SetAlpha(SDL_Surface * surface,
                                         Uint32 flag, Uint8 alpha);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_DisplayFormat(SDL_Surface * surface);
extern __attribute__ ((visibility("default"))) SDL_Surface * SDL_DisplayFormatAlpha(SDL_Surface *
                                                            surface);
extern __attribute__ ((visibility("default"))) void SDL_WM_SetCaption(const char *title,
                                               const char *icon);
extern __attribute__ ((visibility("default"))) void SDL_WM_GetCaption(const char **title,
                                               const char **icon);
extern __attribute__ ((visibility("default"))) void SDL_WM_SetIcon(SDL_Surface * icon, Uint8 * mask);
extern __attribute__ ((visibility("default"))) int SDL_WM_IconifyWindow(void);
extern __attribute__ ((visibility("default"))) int SDL_WM_ToggleFullScreen(SDL_Surface * surface);
extern __attribute__ ((visibility("default"))) SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode);
extern __attribute__ ((visibility("default"))) int SDL_SetPalette(SDL_Surface * surface,
                                           int flags,
                                           const SDL_Color * colors,
                                           int firstcolor, int ncolors);
extern __attribute__ ((visibility("default"))) int SDL_SetColors(SDL_Surface * surface,
                                          const SDL_Color * colors,
                                          int firstcolor, int ncolors);
extern __attribute__ ((visibility("default"))) int SDL_GetWMInfo(struct SDL_SysWMinfo *info);
extern __attribute__ ((visibility("default"))) Uint8 SDL_GetAppState(void);
extern __attribute__ ((visibility("default"))) void SDL_WarpMouse(Uint16 x, Uint16 y);
extern __attribute__ ((visibility("default"))) SDL_Overlay * SDL_CreateYUVOverlay(int width,
                                                          int height,
                                                          Uint32 format,
                                                          SDL_Surface *
                                                          display);
extern __attribute__ ((visibility("default"))) int SDL_LockYUVOverlay(SDL_Overlay * overlay);
extern __attribute__ ((visibility("default"))) void SDL_UnlockYUVOverlay(SDL_Overlay * overlay);
extern __attribute__ ((visibility("default"))) int SDL_DisplayYUVOverlay(SDL_Overlay * overlay,
                                                  SDL_Rect * dstrect);
extern __attribute__ ((visibility("default"))) void SDL_FreeYUVOverlay(SDL_Overlay * overlay);
extern __attribute__ ((visibility("default"))) void SDL_GL_SwapBuffers(void);
extern __attribute__ ((visibility("default"))) int SDL_SetGamma(float red, float green, float blue);
extern __attribute__ ((visibility("default"))) int SDL_SetGammaRamp(const Uint16 * red,
                                             const Uint16 * green,
                                             const Uint16 * blue);
extern __attribute__ ((visibility("default"))) int SDL_GetGammaRamp(Uint16 * red, Uint16 * green,
                                             Uint16 * blue);
extern __attribute__ ((visibility("default"))) int SDL_EnableKeyRepeat(int delay, int interval);
extern __attribute__ ((visibility("default"))) void SDL_GetKeyRepeat(int *delay, int *interval);
extern __attribute__ ((visibility("default"))) int SDL_EnableUNICODE(int enable);

typedef SDL_Window* SDL_WindowID;







typedef Uint32 ( * SDL_OldTimerCallback) (Uint32 interval);
extern __attribute__ ((visibility("default"))) int SDL_SetTimer(Uint32 interval, SDL_OldTimerCallback callback);

extern __attribute__ ((visibility("default"))) int SDL_putenv(const char *variable);
# 361 "../Frameworks/SDL.framework/Headers/SDL_compat.h"
# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 362 "../Frameworks/SDL.framework/Headers/SDL_compat.h" 2
# 94 "../Frameworks/SDL.framework/Headers/SDL.h" 2

# 1 "../Frameworks/SDL.framework/Headers/begin_code.h" 1
# 96 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 126 "../Frameworks/SDL.framework/Headers/SDL.h"
extern __attribute__ ((visibility("default"))) int SDL_Init(Uint32 flags);




extern __attribute__ ((visibility("default"))) int SDL_InitSubSystem(Uint32 flags);




extern __attribute__ ((visibility("default"))) void SDL_QuitSubSystem(Uint32 flags);







extern __attribute__ ((visibility("default"))) Uint32 SDL_WasInit(Uint32 flags);





extern __attribute__ ((visibility("default"))) void SDL_Quit(void);







# 1 "../Frameworks/SDL.framework/Headers/close_code.h" 1
# 159 "../Frameworks/SDL.framework/Headers/SDL.h" 2
# 1 "test.c" 2
