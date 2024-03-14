#include <unistd.h>
#include <sys/stat.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

/* caml_executable_name exists but doesn't support macOS on all compiler versions */
char* fl_caml_executable_name(void) {
#if defined(__linux__)
  int namelen, retcode;
  char * name;
  struct stat st;

  /* lstat("/proc/self/exe") returns st_size == 0 so we cannot use it
     to determine the size of the buffer.  Instead, we guess and adjust. */
  namelen = 256;
  while (1) {
    name = caml_stat_alloc(namelen);
    retcode = readlink("/proc/self/exe", name, namelen);
    if (retcode == -1) { caml_stat_free(name); return NULL; }
    if (retcode < namelen) break;
    caml_stat_free(name);
    if (namelen >= 1024*1024) return NULL; /* avoid runaway and overflow */
    namelen *= 2;
  }
  /* readlink() does not zero-terminate its result.
     There is room for a final zero since retcode < namelen. */
  name[retcode] = 0;
  /* Make sure that the contents of /proc/self/exe is a regular file.
     (Old Linux kernels return an inode number instead.) */
  if (stat(name, &st) == -1 || ! S_ISREG(st.st_mode)) {
    caml_stat_free(name); return NULL;
  }
  return name;

#elif defined(__APPLE__)
  unsigned int namelen;
  char * name;

  namelen = 256;
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  /* Buffer is too small, but namelen now contains the size needed */
  name = caml_stat_alloc(namelen);
  if (_NSGetExecutablePath(name, &namelen) == 0) return name;
  caml_stat_free(name);
  return NULL;

#else
  return NULL;

#endif
}

/* redefinitions of macros and helpers from newer ocaml */
#define Fl_Tag_some 0
#define Fl_Val_none Val_int(0)

CAMLprim value fl_caml_alloc_some(value v)
{
  CAMLparam1(v);
  value some = caml_alloc_small(1, Fl_Tag_some);
  Field(some, 0) = v;
  CAMLreturn(some);
}

CAMLprim value fl_executable_path(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(caml_path, caml_result);
  char* path = fl_caml_executable_name();
  if (path != NULL) {
    caml_path = caml_copy_string(path);
    caml_result = fl_caml_alloc_some(caml_path);
    caml_stat_free(path);
  } else {
    caml_result = Fl_Val_none;
  }
  CAMLreturn(caml_result);
}
