;;;; wrapper.lisp ---- Wrapper of ObjC @try @catch

(in-package :coca.objc)

#+darwin
(progn
  (cc-flags "-ObjC")
  (ld-flags "-framework" "Foundation"))

;; FUTURE: gnustep

(pkg-config-cflags "libffi")

(ld-flags "-lffi")

(import  "Foundation/Foundation.h")

(include "ffi.h")

;;; objc_msgSend
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

;;; libFFI
(objc "
ffi_cif *coca_alloc_ffi_cif (size_t len, ffi_type* ret, ffi_type** atypes) {
  ffi_cif *cif = malloc(sizeof(ffi_cif));
  if (ffi_prep_cif(cif, FFI_DEFAULT_ABI, len, ret, atypes) != FFI_OK)
    return NULL;
  else
    return cif;
}

ffi_type *coca_alloc_struct_ffi_type(ffi_type** elements) {
  ffi_type *type = malloc(sizeof(ffi_type));
  type->size = 0;
  type->alignment = 0;
  type->type = FFI_TYPE_STRUCT;
  type->elements = elements;
  return type;
}
")

;;;; wrapper.lisp ends here
