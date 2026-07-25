;;;; framed.lisp --- Mixin for classes support frame and setFrame: method

(in-package :coca.appkit)

(defclass framed-mixin () ()
  (:documentation
   "Titled mixin for classes support frame and setFrame: method.

Use `frame' to get the frame (`ns-rect');
Use `set-frame' to set the frame size.

ObjC use coordinates where lower-left is origin:

    Y
    ^ |<- W ->|
    | +-------+--
    | |       | ^
    | |       | H
    | |       | V
    | +-------+--
    *-------------> X
   (0, 0)

"))

(defgeneric frame (framed)
  (:documentation
   "Get the frame of FRAMED.
Return values X, Y, W, H. ")
  (:method ((framed framed-mixin))
    (invoke (objc-ptr framed) "frame" :ns-rect)))

(defgeneric set-frame (framed x y w h)
  (:documentation
   "Set the frame of FRAMED.
Return `framed'. ")
  (:method ((framed framed-mixin) x y w h)
    (with-ptr framed ptr
      (dispatch-main ()
        (invoke ptr "setFrame:" :ns-rect (x y w h))))))

;;;; framed.lisp ends here
