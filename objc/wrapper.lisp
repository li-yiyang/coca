;;;; wrapper.lisp ---- Wrapper of ObjC @try @catch

(in-package :coca.objc)

(cc-flags "-ObjC" "-framework" "Foundation" "-lffi"
          "-I/Library/Developer/CommandLineTools/SDKs/MacOSX26.sdk/usr/include/ffi")
(ld-flags "-framework" "Foundation" "-lffi")

(c "
#import  <Foundation/Foundation.h>
#include <ffi.h>

typedef void (*coca_lisp_callback_t)(id);

static coca_lisp_callback_t coca_lisp_exception_handler = NULL;

void set_coca_lisp_exception_handler (coca_lisp_callback_t callback) {
  coca_lisp_exception_handler = callback;
}

void coca_objc_msgSend (ffi_cif *cif, IMP imp, void* retval, void** args) {
  @try {
    return ffi_call(cif, FFI_FN(imp), retval, args);
  }
  @catch (NSException *e) {
    coca_lisp_exception_handler(e);
  }
}
")

(defwrapper (%coca_objc_msgSend "coca_objc_msgSend") :void
  (ffi-cif        :pointer)
  (implementation :pointer)
  (retval         :pointer)
  (args           :pointer))

;;;; wrapper.lisp ends here
