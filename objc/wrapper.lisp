;;;; wrapper.lisp ---- Wrapper of ObjC @try @catch

(in-package :coca.objc)

(cc-flags "-ObjC" "-framework" "Foundation" "-lffi")
(pkg-config-cflags "libffi")

(ld-flags "-framework" "Foundation" "-lffi")

(import  "Foundation/Foundation.h")

(include "ffi.h")

(objc "
typedef void (*coca_lisp_callback_t)(id);

static coca_lisp_callback_t coca_lisp_exception_handler = NULL;

void set_coca_lisp_exception_handler (coca_lisp_callback_t callback) {
  coca_lisp_exception_handler = callback;
}

void coca_objc_msgSend (ffi_cif *cif, IMP imp, void* retval, void** args) {
  @try {
    ffi_call(cif, FFI_FN(imp), retval, args);
  }
  @catch (NSException *e) {
    coca_lisp_exception_handler(e);
  }
}
")

;;;; wrapper.lisp ends here
