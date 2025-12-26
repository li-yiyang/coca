;;;; wrapper.lisp ---- Wrapper of ObjC @try @catch

(in-package :coca.objc)

(cc-flags "-ObjC" "-framework" "Foundation")
(ld-flags "-framework" "Foundation")

(c "
#import \"Foundation/Foundation.h\"

typedef void (*coca_lisp_callback_t)(id);

static coca_lisp_callback_t coca_lisp_exception_callback = NULL;

void set_coca_lisp_exception_callback (coca_lisp_callback_t callback) {
  coca_lisp_exception_callback = callback;
}

void coca_lisp_call_wrapper (void (*call)(void)) {
  @try {
    call();
  }
  @catch (NSException *e) {
    NSLog(@\"C-side caught: %@\", [e reason]);
    if (coca_lisp_exception_callback) {
      coca_lisp_exception_callback(e);
    } else {
      NSLog(@\"This can't be: Unhandled Exception: %@. \", e);
    }
  }
  @catch (id unknown) {
    NSLog(@\"wired\");
  }
}
")

;;;; wrapper.lisp ends here
