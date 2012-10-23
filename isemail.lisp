;;;;
;;;; To validate an email address according to RFCs 5321, 5322 and others
;;;; 
;;;; Copyright (c) 2012, Anthony Yen <Anthony_Yen@AutomaticIT.com>
;;;; Copyright (c) 2008-2011, Dominic Sayers
;;;; Test schema documentation Copyright (c) 2011, Daniel Marschall
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or without modification,
;;;; are permitted provided that the following conditions are met:
;;;; 
;;;;     - Redistributions of source code must retain the above copyright notice,
;;;;       this list of conditions and the following disclaimer.
;;;;     - Redistributions in binary form must reproduce the above copyright notice,
;;;;       this list of conditions and the following disclaimer in the documentation
;;;;       and/or other materials provided with the distribution.
;;;;     - Neither the name of Dominic Sayers nor the names of its contributors may be
;;;;       used to endorse or promote products derived from this software without
;;;;       specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;; 
;;;; @package     is_email
;;;; @author      Dominic Sayers <dominic@sayers.cc>
;;;; @copyright   2008-2011 Dominic Sayers
;;;; @license     http://www.opensource.org/licenses/bsd-license.php BSD License
;;;; @link        http://www.dominicsayers.com/isemail
;;;; @version     3.01.1 - Fixed examples and readme.txt
;;;;
;;;; Lisp port notes:
;;;; 1. PHP equivalancy operator ===, ported as eql.

(defpackage #:com.automaticit.is_email
  (:use #:cl #:alexandria #:cl-ppcre))  ; #:cl-utilities included by alexandria

(in-package #:com.automaticit.is_email)

(ql:quickload "alexandria")
(require :alexandria)
(ql:quickload "cl-ppcre")
(require :cl-ppcre)
;; (ql:quickload "cl-utilities")
;; (require :cl-utilities)

;; Used by PHP, carried over in case the translation is needed for an
;; unforeseen future requirement.
;; http://php.net/manual/en/errorfunc.constants.php
(defconstant E_ERROR   1 "PHP Fatal run-time errors. These indicate errors that can not be recovered from, such as a memory allocation problem. Execution of the script is halted.")
(defconstant E_WARNING 2 "PHP Run-time warnings (non-fatal errors). Execution of the script is not halted.")

;; [TODO] Generate from test/meta.xml by writing a new XSL for Common Lisp
;; Categories
(defconstant ISEMAIL_VALID_CATEGORY   1 "Address is valid")
(defconstant ISEMAIL_DNSWARN          7 "Address is valid but a DNS check was not successful")
(defconstant ISEMAIL_RFC5321         15 "Address is valid for SMTP but has unusual elements")
(defconstant ISEMAIL_CFWS            31 "Address is valid within the message but cannot be used unmodified for the envelope")
(defconstant ISEMAIL_DEPREC          63 "Address contains deprecated elements but may still be valid in restricted contexts")
(defconstant ISEMAIL_RFC5322        127 "The address is only valid according to the broad definition of RFC 5322. It is otherwise invalid.")
(defconstant ISEMAIL_ERR            255 "Address is invalid for any purpose")

;; Categories
;; Address is valid
(defconstant ISEMAIL_VALID 0);
;; Address is valid but a DNS check was not successful
(defconstant ISEMAIL_DNSWARN_NO_MX_RECORD 5)
(defconstant ISEMAIL_DNSWARN_NO_RECORD    6)
;; Address is valid for SMTP but has unusual elements
(defconstant ISEMAIL_RFC5321_TLD             9)
(defconstant ISEMAIL_RFC5321_TLDNUMERIC     10)
(defconstant ISEMAIL_RFC5321_QUOTEDSTRING   11)
(defconstant ISEMAIL_RFC5321_ADDRESSLITERAL 12)
(defconstant ISEMAIL_RFC5321_IPV6DEPRECATED 13)
;; Address is valid within the message but cannot be used unmodified for the envelope
(defconstant ISEMAIL_CFWS_COMMENT 17)
(defconstant ISEMAIL_CFWS_FWS     18)
;; Address contains deprecated elements but may still be valid in restricted contexts
(defconstant ISEMAIL_DEPREC_LOCALPART    33)
(defconstant ISEMAIL_DEPREC_FWS          34)
(defconstant ISEMAIL_DEPREC_QTEXT        35)
(defconstant ISEMAIL_DEPREC_QP           36)
(defconstant ISEMAIL_DEPREC_COMMENT      37)
(defconstant ISEMAIL_DEPREC_CTEXT        38)
(defconstant ISEMAIL_DEPREC_CFWS_NEAR_AT 49)
;; The address is only valid according to the broad definition of RFC 5322. It is otherwise invalid.
(defconstant ISEMAIL_RFC5322_DOMAIN          65)
(defconstant ISEMAIL_RFC5322_TOOLONG         66)
(defconstant ISEMAIL_RFC5322_LOCAL_TOOLONG   67)
(defconstant ISEMAIL_RFC5322_DOMAIN_TOOLONG  68)
(defconstant ISEMAIL_RFC5322_LABEL_TOOLONG   69)
(defconstant ISEMAIL_RFC5322_DOMAINLITERAL   70)
(defconstant ISEMAIL_RFC5322_DOMLIT_OBSDTEXT 71)
(defconstant ISEMAIL_RFC5322_IPV6_GRPCOUNT   72)
(defconstant ISEMAIL_RFC5322_IPV6_2X2XCOLON  73)
(defconstant ISEMAIL_RFC5322_IPV6_BADCHAR    74)
(defconstant ISEMAIL_RFC5322_IPV6_MAXGRPS    75)
(defconstant ISEMAIL_RFC5322_IPV6_COLONSTRT  76)
(defconstant ISEMAIL_RFC5322_IPV6_COLONEND   77)
;; Address is invalid for any purpose
(defconstant ISEMAIL_ERR_EXPECTING_DTEXT    129)
(defconstant ISEMAIL_ERR_NOLOCALPART        130)
(defconstant ISEMAIL_ERR_NODOMAIN           131)
(defconstant ISEMAIL_ERR_CONSECUTIVEDOTS    132)
(defconstant ISEMAIL_ERR_ATEXT_AFTER_CFWS   133)
(defconstant ISEMAIL_ERR_ATEXT_AFTER_QS     134)
(defconstant ISEMAIL_ERR_ATEXT_AFTER_DOMLIT 135)
(defconstant ISEMAIL_ERR_EXPECTING_QPAIR    136)
(defconstant ISEMAIL_ERR_EXPECTING_ATEXT    137)
(defconstant ISEMAIL_ERR_EXPECTING_QTEXT    138)
(defconstant ISEMAIL_ERR_EXPECTING_CTEXT    139)
(defconstant ISEMAIL_ERR_BACKSLASHEND       140)
(defconstant ISEMAIL_ERR_DOT_START          141)
(defconstant ISEMAIL_ERR_DOT_END            142)
(defconstant ISEMAIL_ERR_DOMAINHYPHENSTART  143)
(defconstant ISEMAIL_ERR_DOMAINHYPHENEND    144)
(defconstant ISEMAIL_ERR_UNCLOSEDQUOTEDSTR  145)
(defconstant ISEMAIL_ERR_UNCLOSEDCOMMENT    146)
(defconstant ISEMAIL_ERR_UNCLOSEDDOMLIT     147)
(defconstant ISEMAIL_ERR_FWS_CRLF_X2        148)
(defconstant ISEMAIL_ERR_FWS_CRLF_END       149)
(defconstant ISEMAIL_ERR_CR_NO_LF           150)
;; End of generated code
;; :diagnostic constants end:

;; function control
(defconstant ISEMAIL_THRESHOLD                 16)

;; Email parts
(defconstant ISEMAIL_COMPONENT_LOCALPART       0)
(defconstant ISEMAIL_COMPONENT_DOMAIN          1)
(defconstant ISEMAIL_COMPONENT_LITERAL         2)
(defconstant ISEMAIL_CONTEXT_COMMENT           3)
(defconstant ISEMAIL_CONTEXT_FWS               4)
(defconstant ISEMAIL_CONTEXT_QUOTEDSTRING      5)
(defconstant ISEMAIL_CONTEXT_QUOTEDPAIR        6)

;; Miscellaneous string constants
(define-constant ISEMAIL_STRING_AT                 "@"     :test #'string=)
(define-constant ISEMAIL_STRING_BACKSLASH          "\\"    :test #'string=)
(define-constant ISEMAIL_STRING_DOT                "."     :test #'string=)
(define-constant ISEMAIL_STRING_DQUOTE             "\""    :test #'string=)
(define-constant ISEMAIL_STRING_OPENPARENTHESIS    "("     :test #'string=)
(define-constant ISEMAIL_STRING_CLOSEPARENTHESIS   ")"     :test #'string=)
(define-constant ISEMAIL_STRING_OPENSQBRACKET      "["     :test #'string=)
(define-constant ISEMAIL_STRING_CLOSESQBRACKET     "]"     :test #'string=)
(define-constant ISEMAIL_STRING_HYPHEN             "-"     :test #'string=)
(define-constant ISEMAIL_STRING_COLON              ":"     :test #'string=)
(define-constant ISEMAIL_STRING_DOUBLECOLON        "::"    :test #'string=)
(define-constant ISEMAIL_STRING_SP                 " "     :test #'string=)
(define-constant ISEMAIL_STRING_HTAB               "\t"    :test #'string=)
(define-constant ISEMAIL_STRING_CR                 "\r"    :test #'string=)
(define-constant ISEMAIL_STRING_LF                 "\n"    :test #'string=)
(define-constant ISEMAIL_STRING_IPV6TAG            "IPv6:" :test #'string=)
;; US-ASCII visible characters not valid for atext (http://tools.ietf.org/html/rfc5322#section-3.2.3)
(define-constant ISEMAIL_STRING_SPECIALS           "()<>[]:\;@\\,.\"" :test #'string=)

;;;
;;; Check that an email address conforms to RFCs 5321, 5322 and others
;;; 
;;; As of Version 3.0, we are now distinguishing clearly between a Mailbox
;;; as defined by RFC 5321 and an addr-spec as defined by RFC 5322. Depending
;;; on the context, either can be regarded as a valid email address. The
;;; RFC 5321 Mailbox specification is more restrictive (comments, white space
;;; and obsolete forms are not allowed)
;;; 
;;; @param string        $email          The email address to check
;;; @param boolean       $checkDNS       If true then a DNS check for MX records will be made
;;; @param mixed         $errorlevel     Determines the boundary between valid and invalid addresses.
;;;                                      Status codes above this number will be returned as-is,
;;;                                      status codes below will be returned as ISEMAIL_VALID. Thus the
;;;                                      calling program can simply look for ISEMAIL_VALID if it is
;;;                                      only interested in whether an address is valid or not. The
;;;                                      errorlevel will determine how "picky" is_email() is about
;;;                                      the address.
;;; 
;;;                                      If omitted or passed as false then is_email() will return
;;;                                      true or false rather than an integer error or warning.
;;; 
;;;                                      NB Note the difference between $errorlevel = false and
;;;                                      $errorlevel = 0
;;; @param array         $parsedata      If passed, returns the parsed address components
;;;

;; Reproduce PHP is_bool() function.
;; Return T   iff variable-to-evaluate is value T or NIL.
;; Return NIL iff variable-to-evaluate is not value T or NIL.
(defun is_bool (variable-to-evaluate)
  (cond
    ((eql variable-to-evaluate t)   t  )
    ((eql variable-to-evaluate nil) t  )
    (t                              nil)))


;; Note: Call with parsedata as a mutable list if caller needs
;; elements of email address separated into a vector.
(defun is_email (email &optional (checkDNS nil) (errorlevel nil) parsedata)
  "Original PHP function signature
   .mixed.*/ function is_email($email, $checkDNS = false, $errorlevel = false, &$parsedata = array()) {
           // Check that $email is a valid address. Read the following RFCs to understand the constraints:
           //      (http://tools.ietf.org/html/rfc5321)
           //      (http://tools.ietf.org/html/rfc5322)
           //      (http://tools.ietf.org/html/rfc4291#section-2.2)
           //      (http://tools.ietf.org/html/rfc1123#section-2.1)
           //      (http://tools.ietf.org/html/rfc3696) (guidance only)
      version 2.0: Enhance $diagnose parameter to $errorlevel
      version 3.0: Introduced status categories
      revision 3.1: BUG: $parsedata was passed by value instead of by reference
   "
  ;; Local scope variables in the order first used in function.
  (let (
        diagnose
        threshold
        return_status
        raw_length
        context
        context_stack
        context_prior
        token
        token_prior
        atomlist
        element_count
        element_len
        hyphen_flag
        end_or_die
        ord
        max_groups
        matchesIP
        index
        addressliteral
        pcre-match-start pcre-match-end pcre-reg-starts pcre-reg-ends
        IPv6
        groupCount
        status
        )
    (declare (optimize (debug 3)))
    (break "Start of function")
    (if (is_bool errorlevel)
        (cond ((eql errorlevel nil  ) (setf diagnose nil))
              ((eql errorlevel 0    ) (setf diagnose nil))
              ((eql errorlevel 0.0  ) (setf diagnose nil))
              ((eql errorlevel ""   ) (setf diagnose nil))
              ((eql errorlevel "0"  ) (setf diagnose nil))
              ((eql errorlevel "0.0") (setf diagnose nil))
              ((eql errorlevel ()   ) (setf diagnose nil))
              (t                      (setf diagnose t)))
        (progn (setf diagnose t)
               (cond ((eql errorlevel E_WARNING) (setf threshold ISEMAIL_THRESHOLD))
                     ((eql errorlevel E_ERROR  ) (setf threshold ISEMAIL_VALID)    )
                     (t                          (setf threshold (coerce errorlevel 'integer))))))
    (setf return_status (make-array ISEMAIL_VALID :fill-pointer t :adjustable t))

    ;; Parse the address into components, character by character
    (setf raw_length    (length email))
    (setf context       ISEMAIL_COMPONENT_LOCALPART)        ; Where we are
;;    (setf context_stack '(context))                         
    (setf context_stack
          (make-array context
                      :fill-pointer t
                      :adjustable   t))                     ; Where we have been
    (setf context_prior	ISEMAIL_COMPONENT_LOCALPART)        ; Where we just came from
    ;; (setf token         #\Space)
    (setf token         nil)                                ; The current character
    (setf token_prior	nil)                                ; The previous character
    (setf parsedata (make-array ISEMAIL_COMPONENT_LITERAL
                                :fill-pointer  t
                                :adjustable    t))          ; For the components of the address
    (setf (aref parsedata ISEMAIL_COMPONENT_LOCALPART) "")
    (setf (aref parsedata ISEMAIL_COMPONENT_DOMAIN   ) "")
;;    (setf parsedata '((ISEMAIL_COMPONENT_LOCALPART "")
;;                      (ISEMAIL_COMPONENT_DOMAIN    "")))
    (setf atomlist '(ISEMAIL_COMPONENT_LITERAL ))
    (setf atomlist (make-array ISEMAIL_COMPONENT_LITERAL))
                               ;; :fill-pointer t
                               ;; :adjustable   t))            ; For the dot-atom elements of the address
    (setf (aref atomlist ISEMAIL_COMPONENT_LOCALPART) (make-array 0 :fill-pointer t :adjustable t))
    (setf (aref atomlist ISEMAIL_COMPONENT_DOMAIN   ) (make-array 0 :fill-pointer t :adjustable t))
;;    (setf atomlist '((ISEMAIL_COMPONENT_LOCALPART '(""))
;;                     (ISEMAIL_COMPONENT_DOMAIN    '(""))))  
    (setf element_count 0)
    (setf element_len   0)
    (setf hyphen_flag   nil)                                ; Hyphen cannot occur at the end of a subdomain
    (setf end_or_die    nil)                                ; CFWS can only appear at the end of the element
    (dotimes (i raw_length)
      (print "top of loop")
      (setf token (char email i))
      (concatenate 'string "test" )
      (cond ((eql context ISEMAIL_COMPONENT_LOCALPART)
             ;;-------------------------------------------------------------
             ;; local-part
             ;;-------------------------------------------------------------
             ;; http://tools.ietf.org/html/rfc5322#section-3.4.1
             ;;   local-part      =   dot-atom / quoted-string / obs-local-part
             ;;
             ;;   dot-atom        =   [CFWS] dot-atom-text [CFWS]
             ;;
             ;;   dot-atom-text   =   1*atext *("." 1*atext)
             ;;
             ;;   quoted-string   =   [CFWS]
             ;;                       DQUOTE *([FWS] qcontent) [FWS] DQUOTE
             ;;                       [CFWS]
             ;;
             ;;   obs-local-part  =   word *("." word)
             ;;
             ;;   word            =   atom / quoted-string
             ;;
             ;;   atom            =   [CFWS] 1*atext [CFWS]
             (cond ((char-equal token ISEMAIL_STRING_OPENPARENTHESIS)
                    (if (eql element_len 0)
                        (progn (print "ISEMAIL_STRING_OPENPARENTHESIS then branch")
                               (if (eql element_count 0) (vector-push-extend ISEMAIL_CFWS_COMMENT return_status) (vector-push-extend ISEMAIL_DEPREC_COMMENT return_status)))
                        (progn (print "ISEMAIL_STRING_OPENPARENTHESIS else branch")
                               (setf end_or_die t)))
                    (vector-push-extend context context_stack)
                    (setf context ISEMAIL_CONTEXT_COMMENT)
                    :ISEMAIL_STRING_OPENPARENTHESIS)
                   
                   ;; Next dot-atom element
                   ((char-equal token ISEMAIL_STRING_DOT)
                    (if (eql element_len 0)
                        ;; Another dot, already?
                        (if (eql element_count 0) (vector-push-extend ISEMAIL_ERR_DOT_START return_status) (vector-push-extend ISEMAIL_ERR_CONSECUTIVEDOTS return_status))  ; Fatal error
                        (progn
                          ;; The entire local-part can be a quoted string for RFC 5321
                          ;; If it's just one atom that is quoted then it's an RFC 5322 obsolete form
                          (when (eql end_or_die t) (vector-push-extend ISEMAIL_DEPREC_LOCALPART return_status))
                          (setf end_or_die 'nil)                         ; CFWS & quoted strings are OK again now we're at the beginning of an element (although they are obsolete forms)
                          (setf element_len 0)
                          (incf element_count)
                          (vector-push-extend token (aref parsedata ISEMAIL_COMPONENT_LOCALPART))
                          (vector-push-extend ""    (aref (aref atomlist ISEMAIL_COMPONENT_LOCALPART) element_count))))
                    :ISEMAIL_STRING_DOT)

                   ;; Quoted string
                   ((char-equal token ISEMAIL_STRING_DQUOTE)
                    (if (eql element_len 0)
                        ;; The entire local-part can be a quoted string for RFC 5321
                        ;; If it's just one atom that is quoted then it's an RFC 5322 obsolete form
                        (progn (if (eql element_count 0) (vector-push-extend ISEMAIL_RFC5321_QUOTEDSTRING return_status) (vector-push-extend ISEMAIL_DEPREC_LOCALPART return_status))
                               (vector-push-extend token (aref parsedata      ISEMAIL_COMPONENT_LOCALPART))
                               (vector-push-extend token (aref (aref atomlist ISEMAIL_COMPONENT_LOCALPART) element_count))
                               ;; (concatenate 'string (aref parsedata      ISEMAIL_COMPONENT_LOCALPART)                token)
                               ;; (concatenate 'string (aref (aref atomlist ISEMAIL_COMPONENT_LOCALPART) element_count) token)
                               (incf element_len)
                               (setf end_or_die 't)                      ; Quoted string must be the entire element
                               (vector-push-extend context context_stack)
                               (setf               context ISEMAIL_CONTEXT_QUOTEDSTRING))
                        (vector-push-extend ISEMAIL_ERR_EXPECTING_ATEXT return_status))        ; Fatal error
                    :ISEMAIL_STRING_DQUOTE)

                   ;; Folding White Space
                   ((or (char-equal token ISEMAIL_STRING_CR) (char-equal token ISEMAIL_STRING_SP) (char-equal token ISEMAIL_STRING_HTAB))
                    (progn (if (and (char-equal token ISEMAIL_STRING_CR) (or (eql (+ i 1) raw_length) (not (char-equal (aref email i) ISEMAIL_STRING_LF))))
                               (vector-push-extend ISEMAIL_ERR_CR_NO_LF return_status))  ; Fatal error

                           (if (eql element_len 0)
                               (if (eql element_count 0) (vector-push-extend ISEMAIL_CFWS_FWS return_status) (vector-push-extend ISEMAIL_DEPREC_FWS return_status))
                               (setf end_or_die 't))                     ; We can't start FWS in the middle of an element, so this better be the end
                           (vector-push-extend context     context_stack)
                           (setf               context     ISEMAIL_CONTEXT_FWS)
                           (setf               token_prior token))
                    :FOLDING_WHITE_SPACE)

                   ;; @ (at sign)
                   ((char-equal token ISEMAIL_STRING_AT)
                    ;; At this point we should have a valid local-part
                    (progn (if (/= (array-dimension context_stack 0) 1) (break "Unexpected item on context stack"))
                           (cond ((char-equal (aref parsedata ISEMAIL_COMPONENT_LOCALPART) nil)
                                  (vector-push-extend ISEMAIL_ERR_NOLOCALPART return_status))     ; Fatal error

                                 ((eql element_len 0)
                                  (vector-push-extend ISEMAIL_ERR_DOT_END return_status))       ; Fatal error

                                 ;; http://tools.ietf.org/html/rfc5321#section-4.5.3.1.1
                                 ;; The maximum total length of a user name or other local-part is 64
                                 ;; octets.
                                 ((> (length (sb-ext:string-to-octets (aref parsedata ISEMAIL_COMPONENT_LOCALPART))) 64)
                                  (vector-push-extend ISEMAIL_RFC5322_LOCAL_TOOLONG return_status))


                                 ;; http://tools.ietf.org/html/rfc5322#section-3.4.1
                                 ;;   Comments and folding white space
                                 ;;   SHOULD NOT be used around the "@" in the addr-spec.
                                 ;;
                                 ;; http://tools.ietf.org/html/rfc2119
                                 ;; 4. SHOULD NOT   This phrase, or the phrase "NOT RECOMMENDED" mean that
                                 ;;    there may exist valid reasons in particular circumstances when the
                                 ;;    particular behavior is acceptable or even useful, but the full
                                 ;;    implications should be understood and the case carefully weighed
                                 ;;    before implementing any behavior described with this label.
                                 ((or (eql context_prior ISEMAIL_CONTEXT_COMMENT) (eql context_prior ISEMAIL_CONTEXT_FWS))
                                  (vector-push-extend ISEMAIL_DEPREC_CFWS_NEAR_AT return_status)
                                  ;; Clear everything down for the domain parsing
                                  (setf context       ISEMAIL_COMPONENT_DOMAIN)                                 ; Where we are
                                  (setf context_stack (make-array context :fill-pointer t :adjustable t))       ; Where we have been
                                  (setf element_count 0)
                                  (setf element_len   0)
                                  (setf end_or_die    'nil))))                      ; CFWS can only appear at the end of the element
                    :ISEMAIL_STRING_AT)

                   ;; atext (address text)
                   (t
                    ;; http://tools.ietf.org/html/rfc5322#section-3.2.3
                    ;;    atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                    ;;                        "!" / "#" /        ;  characters not including
                    ;;                        "$" / "%" /        ;  specials.  Used for atoms.
                    ;;                        "&" / "'" /
                    ;;                        "*" / "+" /
                    ;;                        "-" / "/" /
                    ;;                        "=" / "?" /
                    ;;                        "^" / "_" /
                    ;;                        "`" / "{" /
                    ;;                        "|" / "}" /
                    ;;                        "~"
                    (if (eql end_or_die t)
                        ;; We have encountered atext where it is no longer valid
                        (cond ((or (eql context_prior ISEMAIL_CONTEXT_COMMENT) (eql context_prior ISEMAIL_CONTEXT_FWS))
                               (vector-push-extend ISEMAIL_ERR_ATEXT_AFTER_CFWS return_status))
                              
                              ((eql context_prior ISEMAIL_CONTEXT_QUOTEDSTRING)
                               (vector-push-extend ISEMAIL_ERR_ATEXT_AFTER_QS return_status))

                              (t
                               (break "More atext found where none is allowed, but unrecognised prior context: ~:S" '(context_prior))))
                        (progn (setf context_prior            context)
                               (setf ord           (char-code token))
                               (if (or (< ord 33) (> ord 126) (eq ord 10) (not (is_bool (position token ISEMAIL_STRING_SPECIALS))))
                               ;; (if (< ord 33)
                                   (vector-push-extend ISEMAIL_ERR_EXPECTING_ATEXT return_status))   ; Fatal error
                               (vector-push-extend token (aref       parsedata ISEMAIL_COMPONENT_LOCALPART               ))
                               (vector-push-extend token (aref (aref atomlist  ISEMAIL_COMPONENT_LOCALPART) element_count))
                               ;; (concatenate 'string (aref parsedata ISEMAIL_COMPONENT_LOCALPART              ) token)
                               ;; (concatenate 'string (aref atomlist  ISEMAIL_COMPONENT_LOCALPART element_count) token)
                               (incf element_len)))
                    :ADDRESS-TEXT)))
             
            ;;-------------------------------------------------------------
            ;; Domain
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_COMPONENT_DOMAIN)
             ;; http://tools.ietf.org/html/rfc5322#section-3.4.1
             ;;   domain          =   dot-atom / domain-literal / obs-domain
             ;;
             ;;   dot-atom        =   [CFWS] dot-atom-text [CFWS]
             ;;
             ;;   dot-atom-text   =   1*atext *("." 1*atext)
             ;;
             ;;   domain-literal  =   [CFWS] "[" *([FWS] dtext) [FWS] "]" [CFWS]
             ;;
             ;;   dtext           =   %d33-90 /          ; Printable US-ASCII
             ;;                       %d94-126 /         ;  characters not including
             ;;                       obs-dtext          ;  "[", "]", or "\"
             ;;
             ;;   obs-domain      =   atom *("." atom)
             ;;
             ;;   atom            =   [CFWS] 1*atext [CFWS]


             ;; http://tools.ietf.org/html/rfc5321#section-4.1.2
             ;;   Mailbox        = Local-part "@" ( Domain / address-literal )
             ;;
             ;;   Domain         = sub-domain *("." sub-domain)
             ;;
             ;;   address-literal  = "[" ( IPv4-address-literal /
             ;;                    IPv6-address-literal /
             ;;                    General-address-literal ) "]"
             ;;                    ; See Section 4.1.3

             ;; http://tools.ietf.org/html/rfc5322#section-3.4.1
             ;;      Note: A liberal syntax for the domain portion of addr-spec is
             ;;      given here.  However, the domain portion contains addressing
             ;;      information specified by and used in other protocols (e.g.,
             ;;      [RFC1034], [RFC1035], [RFC1123], [RFC5321]).  It is therefore
             ;;      incumbent upon implementations to conform to the syntax of
             ;;      addresses for the context in which they are used.
             ;; is_email() author's note: it's not clear how to interpret this in
             ;; the context of a general email address validator. The conclusion I
             ;; have reached is this: "addressing information" must comply with
             ;; RFC 5321 (and in turn RFC 1035), anything that is "semantically
             ;; invisible" must comply only with RFC 5322.
             (cond ((char-equal token ISEMAIL_STRING_OPENPARENTHESIS)
                    (if (eql element_len 0)
                        ;; Comments at the start of the domain are deprecated in the text
                        ;; Comments at the start of a subdomain are obs-domain
                        ;; (http://tools.ietf.org/html/rfc5322#section-3.4.1)
                        (if (eql element_count 0)
                            (vector-push-extend ISEMAIL_DEPREC_CFWS_NEAR_AT return_status)
                            (vector-push-extend ISEMAIL_DEPREC_COMMENT      return_status))
                        (progn (vector-push-extend ISEMAIL_CFWS_COMMENT return_status)
                               (setf end_or_die t)))    ; We can't start a comment in the middle of an element, so this better be the end
                    (vector-push-extend context context_stack)
                    (setf context ISEMAIL_CONTEXT_COMMENT)
                    :ISEMAIL_STRING_OPENPARENTHESIS
                    )

                   ;; Next dot-atom element
                   ((char-equal token ISEMAIL_STRING_DOT)
                    (cond ((eql element_len 0)
                           ;; Another dot, already?
                           (if (eql element_count 0)
                               (vector-push-extend ISEMAIL_ERR_DOT_START       return_status)
                               (vector-push-extend ISEMAIL_ERR_CONSECUTIVEDOTS return_status))) ; Fatal error

                          ((eql hyphen_flag t)
                           ;; Previous subdomain ended in a hyphen
                           (vector-push-extend ISEMAIL_ERR_DOMAINHYPHENEND     return_status))  ; Fatal error

                          (t
                           ;; Nowhere in RFC 5321 does it say explicitly that the
                           ;; domain part of a Mailbox must be a valid domain according
                           ;; to the DNS standards set out in RFC 1035, but this *is*
                           ;; implied in several places. For instance, wherever the idea
                           ;; of host routing is discussed the RFC says that the domain
                           ;; must be looked up in the DNS. This would be nonsense unless
                           ;; the domain was designed to be a valid DNS domain. Hence we
                           ;; must conclude that the RFC 1035 restriction on label length
                           ;; also applies to RFC 5321 domains.
                           ;;
                           ;; http://tools.ietf.org/html/rfc1035#section-2.3.4
                           ;; labels          63 octets or less
                           (if (> element_len 63)
                               (vector-push-extend ISEMAIL_RFC5322_LABEL_TOOLONG return_status))

                           (setf end_or_die  nil)   ; CFWS is OK again now we're at the beginning of an element (although it may be obsolete CFWS)
                           (setf element_len 0)
                           (incf element_count)
                           (setf (aref (aref atomlist ISEMAIL_COMPONENT_DOMAIN) element_count) "")
                           (vector-push-extend token (aref parsedata ISEMAIL_COMPONENT_DOMAIN))
                           )
                        )
                    (break "ISEMAIL_STRING_DOT")
                    :ISEMAIL_STRING_DOT)

                   ;; Domain literal
                   ((char-equal token ISEMAIL_STRING_OPENSQBRACKET)
                    (if (char-equal (aref parsedata ISEMAIL_COMPONENT_DOMAIN) "")
                        (progn (setf end_or_die t)  ; Domain literal must be the only component
                               (incf element_len)
                               (vector-push-extend context context_stack)
                               (vector-push-extend token   (aref       parsedata ISEMAIL_COMPONENT_DOMAIN))
                               (vector-push-extend token   (aref (aref atomlist  ISEMAIL_COMPONENT_DOMAIN) element_count))
                               (setf (aref parsedata ISEMAIL_COMPONENT_LITERAL) ""))
                        (vector-push-extend ISEMAIL_ERR_EXPECTING_ATEXT return_status))     ; Fatal error
                    (break "ISEMAIL_STRING_OPENSQBRACKET")
                    :ISEMAIL_STRING_OPENSQBRACKET)

                   ;; Folding White Space
                   ((or (char-equal token ISEMAIL_STRING_CR) (char-equal token ISEMAIL_STRING_SP) (char-equal token ISEMAIL_STRING_HTAB))
                    (if (and (char-equal token ISEMAIL_STRING_CR) (or (eql (+ 1 i) raw_length) (not (char-equal (aref email i) ISEMAIL_STRING_LF))))
                    ;; if (($token === ISEMAIL_STRING_CR) && ((++$i === $raw_length) || ($email[$i] !== ISEMAIL_STRING_LF))) {$return_status[] = ISEMAIL_ERR_CR_NO_LF;	break;}	// Fatal error
                        (vector-push-extend ISEMAIL_ERR_CR_NO_LF return_status))    ; Fatal error

                    (if (eql element_len 0)
                        (if (eql element_count 0)
                            (vector-push-extend ISEMAIL_DEPREC_CFWS_NEAR_AT return_status)
                            (vector-push-extend ISEMAIL_DEPREC_FWS          return_status))
                        (progn (vector-push-extend ISEMAIL_CFWS_FWS return_status)
                               (setf end_or_die t)))
                    (vector-push-extend context context_stack)
                    (setf context     ISEMAIL_CONTEXT_FWS)
                    (setf token_prior token)
                    :FOLDING_WHITE_SPACE)

                   ;; atext
                   (t
                    ;; RFC 5322 allows any atext...
                    ;; http://tools.ietf.org/html/rfc5322#section-3.2.3
                    ;;    atext           =   ALPHA / DIGIT /    ; Printable US-ASCII
                    ;;                        "!" / "#" /        ;  characters not including
                    ;;                        "$" / "%" /        ;  specials.  Used for atoms.
                    ;;                        "&" / "'" /
                    ;;                        "*" / "+" /
                    ;;                        "-" / "/" /
                    ;;                        "=" / "?" /
                    ;;                        "^" / "_" /
                    ;;                        "`" / "{" /
                    ;;                        "|" / "}" /
                    ;;                        "~"

                    ;; But RFC 5321 only allows letter-digit-hyphen to comply with DNS rules (RFCs 1034 & 1123)
                    ;; http://tools.ietf.org/html/rfc5321#section-4.1.2
                    ;;   sub-domain     = Let-dig [Ldh-str]
                    ;;
                    ;;   Let-dig        = ALPHA / DIGIT
                    ;;
                    ;;   Ldh-str        = *( ALPHA / DIGIT / "-" ) Let-dig
                    ;;
                    (if (eql end_or_die t)
                        ;; We have encountered atext where it is no longer valid
                        (cond ((or (eql context_prior ISEMAIL_CONTEXT_COMMENT) (eql context_prior ISEMAIL_CONTEXT_FWS))
                               (vector-push-extend ISEMAIL_ERR_ATEXT_AFTER_CFWS return_status))

                              ((eql context_prior ISEMAIL_COMPONENT_LITERAL)
                               (vector-push-extend ISEMAIL_ERR_ATEXT_AFTER_DOMLIT return_status))

                              (t
                               (break "More atext found where none is allowed, but unrecognised prior context: ~:S" '(context_prior)))))

                    (setf ord         (char-code token))
                    (setf hyphen_flag            nil)       ; Assume this token isn't a hyphen unless we discover it is

                    (cond ((or (< ord 33) (> ord 126) (not (is_bool (position token ISEMAIL_STRING_SPECIALS))))
                           (vector-push-extend ISEMAIL_ERR_EXPECTING_ATEXT return_status)
                           :ISBOOL-33-126)

                          ((char-equal token ISEMAIL_STRING_HYPHEN)
                           (if (eql element_len 0)
                               ;; Hyphens can't be at the beginning of a subdomain
                               (vector-push-extend ISEMAIL_ERR_DOMAINHYPHENSTART return_status))    ; Fatal error
                           (setf hyphen_flag t)
                           :ISEMAIL_STRING_HYPHEN)

;;                                         +-----------------------------------------------------------------------------------+
;;                                         | +----------------------+    +----------------------+    +-----------------------+ |
;; 				} elseif (!( ($ord > 47 && $ord < 58) || ($ord > 64 && $ord < 91) || ($ord > 96 && $ord < 123) )) {
                          ((not (or (and (> ord 47) (< ord 58)) (and (> ord 64) (< ord 91)) (and (> ord 96) (< ord 123))))
                           ;; Not an RFC 5321 subdomain, but still OK by RFC 5322
                           (vector-push-extend ISEMAIL_RFC5322_DOMAIN return_status)
                           :ISEMAIL_RFC5322_DOMAIN))

                    (vector-push-extend token (aref       parsedata ISEMAIL_COMPONENT_DOMAIN))
                    (vector-push-extend token (aref (aref atomlist  ISEMAIL_COMPONENT_DOMAIN) element_count))
                    (incf element_len)
                    :ADDRESS-TEXT)
                   )

             )


            ;;-------------------------------------------------------------
            ;; Domain literal
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_COMPONENT_LITERAL)
             (cond ((char-equal token ISEMAIL_STRING_CLOSESQBRACKET)
                    (if (< (reduce #'max return_status) ISEMAIL_DEPREC)
                        ;; Could be a valid RFC 5321 address literal, so let's check

                        ;; http://tools.ietf.org/html/rfc5321#section-4.1.2
                        ;;   address-literal  = "[" ( IPv4-address-literal /
                        ;;                    IPv6-address-literal /
                        ;;                    General-address-literal ) "]"
                        ;;                    ; See Section 4.1.3
                        ;;
                        ;; http://tools.ietf.org/html/rfc5321#section-4.1.3
                        ;;   IPv4-address-literal  = Snum 3("."  Snum)
                        ;;
                        ;;   IPv6-address-literal  = "IPv6:" IPv6-addr
                        ;;
                        ;;   General-address-literal  = Standardized-tag ":" 1*dcontent
                        ;;
                        ;;   Standardized-tag  = Ldh-str
                        ;;                     ; Standardized-tag MUST be specified in a
                        ;;                     ; Standards-Track RFC and registered with IANA
                        ;;
                        ;;   dcontent       = %d33-90 / ; Printable US-ASCII
                        ;;                  %d94-126 ; excl. "[", "\", "]"
                        ;;
                        ;;   Snum           = 1*3DIGIT
                        ;;                  ; representing a decimal integer
                        ;;                  ; value in the range 0 through 255
                        ;;
                        ;;   IPv6-addr      = IPv6-full / IPv6-comp / IPv6v4-full / IPv6v4-comp
                        ;;
                        ;;   IPv6-hex       = 1*4HEXDIG
                        ;;
                        ;;   IPv6-full      = IPv6-hex 7(":" IPv6-hex)
                        ;;
                        ;;   IPv6-comp      = [IPv6-hex *5(":" IPv6-hex)] "::"
                        ;;                  [IPv6-hex *5(":" IPv6-hex)]
                        ;;                  ; The "::" represents at least 2 16-bit groups of
                        ;;                  ; zeros.  No more than 6 groups in addition to the
                        ;;                  ; "::" may be present.
                        ;;
                        ;;   IPv6v4-full    = IPv6-hex 5(":" IPv6-hex) ":" IPv4-address-literal
                        ;;
                        ;;   IPv6v4-comp    = [IPv6-hex *3(":" IPv6-hex)] "::"
                        ;;                  [IPv6-hex *3(":" IPv6-hex) ":"]
                        ;;                  IPv4-address-literal
                        ;;                  ; The "::" represents at least 2 16-bit groups of
                        ;;                  ; zeros.  No more than 4 groups in addition to the
                        ;;                  ; "::" and IPv4-address-literal may be present.
                        ;;
                        ;; is_email() author's note: We can't use ip2long() to validate
                        ;; IPv4 addresses because it accepts abbreviated addresses
                        ;; (xxx.xxx.xxx), expanding the last group to complete the address.
                        ;; filter_var() validates IPv6 address inconsistently (up to PHP 5.3.3
                        ;; at least) -- see http://bugs.php.net/bug.php?id=53236 for example
                        (progn (setf max_groups       8)
                               (setf matchesIP        (make-array 0 :fill-pointer t :adjustable t))
                               (setf pcre-match-start nil)
                               (setf pcre-match-end   nil)
                               (setf pcre-reg-starts  (make-array 0 :fill-pointer t :adjustable t))
                               (setf pcre-reg-ends    (make-array 0 :fill-pointer t :adjustable t))
                               (setf status           (make-array 0 :fill-pointer t :adjustable t))
                               (setf index            nil)
                               (setf addressliteral   (aref parsedata ISEMAIL_COMPONENT_LITERAL))

                               ;; Extract IPv4 part from the end of the address-literal (if there is one)
                               ;; preg_match('/\\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/', $addressliteral, $matchesIP) > 0
                               (multiple-value-bind (pcre-match-start pcre-match-end pcre-reg-starts pcre-reg-ends)
                                   (scan "\\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$" addressliteral)
                                 (declare (ignore                                    pcre-reg-starts pcre-reg-ends))
                                 (if (>= pcre-match-start 0)
                                     (progn (vector-push-extend "" matchesIP)
                                            (setf (aref matchesIP 0) (subseq addressliteral pcre-match-start pcre-match-end))
                                            (setf index (search matchesIP addressliteral :from-end t))
                                            (if (/= index 0)
                                                (concatenate addressliteral (subseq addressliteral 0 index) "0:0"))))  ; Convert IPv4 part to IPv6 format for further testing

                                 (if (eql index 0)
                                     ;; Nothing there except a valid IPv4 address, so...
                                     (vector-push-extend ISEMAIL_RFC5321_ADDRESSLITERAL return_status)
                                     (if (not (equalp addressliteral ISEMAIL_STRING_IPV6TAG))
                                         (vector-push-extend ISEMAIL_RFC5322_DOMAINLITERAL return_status)
                                         (progn (setf IPv6       (subseq addressliteral 0 4))
                                                (mapcar #'(lambda (x) (vector-push-extend x matchesIP)) (split ISEMAIL_STRING_COLON IPv6))  ; Revision 2.7: Daniel Marschall's new IPv6 testing strategy
                                                ;; (setf matchesIP  (split ISEMAIL_STRING_COLON IPv6))  ; Revision 2.7: Daniel Marschall's new IPv6 testing strategy
                                                (setf groupCount (length matchesIP))
                                                (setf index      (position ISEMAIL_STRING_DOUBLECOLON IPv6))

                                                (if (eql index nil)
                                                    ;; We need exactly the right number of groups
                                                    (if (/= groupCount max_groups)
                                                        (vector-push-extend ISEMAIL_RFC5322_IPV6_GRPCOUNT return_status)
                                                        (if (or (eql nil (search ISEMAIL_STRING_DOUBLECOLON IPv6)) (/= index (search ISEMAIL_STRING_DOUBLECOLON IPv6)))
                                                            (vector-push-extend ISEMAIL_RFC5322_IPV6_2X2XCOLON return_status)
                                                            (progn (if (or (eql index 0) (eql index (- (length IPv6) 2)))
                                                                       (incf max_groups)) ; RFC 4291 allows :: at the start or end of an address with 7 other groups in addition
                                                                   
                                                                   (cond ((> groupCount max_groups)
                                                                          (adjust-array status 1)
                                                                          (vector-push-extend ISEMAIL_RFC5322_IPV6_MAXGRPS   return_status)
                                                                          :GROUPCOUNT-GREATER-THAN-MAX_GROUPS)

                                                                         ((eql groupCount max_groups)
                                                                          (vector-push-extend ISEMAIL_RFC5321_IPV6DEPRECATED return_status) ; Eliding a single "::"
                                                                          :GROUPCOUNT-EQL-MAX_GROUPS))))))
                                                
                                                ;; Revision 2.7: Daniel Marschall's new IPv6 testing strategy
                                                (cond ((and (eql (subseq IPv6 0 1) ISEMAIL_STRING_COLON) (char-equal (subseq IPv6 1 2) ISEMAIL_STRING_COLON))
                                                       (vector-push-extend ISEMAIL_RFC5322_IPV6_COLONSTRT return_status) ; Address starts with a single colon
                                                       :ADDRESS-STARTS-WITH-SINGLE-COLON)

                                                      ((and (eql (subseq IPv6 (- (length IPv6) 1) (length IPv6)) ISEMAIL_STRING_COLON) (string-equal (subseq IPv6 (- (length IPv6) 2) (- (length IPv6) 1)) ISEMAIL_STRING_COLON))
                                                       (vector-push-extend ISEMAIL_RFC5322_IPV6_COLONEND  return_status) ; Address ends with a single colon
                                                       :ADDRESS-ENDS-WITH-SINGLE-COLON)

;; 						elseif (count(preg_grep('/^[0-9A-Fa-f]{0,4}$/', $matchesIP, PREG_GREP_INVERT)) !== 0)
                                                      ((/= (length (delete-if-not #'(lambda (x) (eql x t)) (mapcar #'(lambda (x) (not (scan "^[0-9A-Fa-f]{0,4}" x))) matchesIP))) 0)
                                                       (vector-push-extend ISEMAIL_RFC5322_IPV6_BADCHAR   return_status) ; Check for unmatched characters
                                                       :ADDRESS-UNMATCHED-CHARS)

                                                      (t
                                                       (vector-push-extend ISEMAIL_RFC5321_ADDRESSLITERAL return_status)
                                                       :ADDRESS-IS-LITERAL)
                                                      ))))))
                        (vector-push-extend ISEMAIL_RFC5322_DOMAINLITERAL return_status))

                    (vector-push-extend token (aref parsedata ISEMAIL_COMPONENT_DOMAIN))
                    (vector-push-extend token (aref (aref atomlist ISEMAIL_COMPONENT_DOMAIN) element_count))
                    (incf element_len)
                    (setf context_prior context)
                    (setf context (vector-pop context_stack))
                    :ISEMAIL_STRING_CLOSESQBRACKET)

                   ((char-equal token ISEMAIL_STRING_BACKSLASH)
                    (vector-push-extend ISEMAIL_RFC5322_DOMLIT_OBSDTEXT return_status)
                    (vector-push-extend context                         context_stack)
                    (setf               context                         ISEMAIL_CONTEXT_QUOTEDPAIR)
                    :ISEMAIL_STRING_BACKSLASH)

                   ;; Folding White Space
                   ((or (char-equal token ISEMAIL_STRING_CR) (char-equal token ISEMAIL_STRING_SP) (char-equal token ISEMAIL_STRING_HTAB))
                    t
                    :FOLDING_WHITE_SPACE)

                   ;; dtext
                   (t
                    (break "default")
                    :DEFAULT-DOMAIN-LITERAL)
                   )
)


            ;;-------------------------------------------------------------
            ;; Quoted string
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_CONTEXT_QUOTEDSTRING)
             t)


            ;;-------------------------------------------------------------
            ;; Quoted pair
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_CONTEXT_QUOTEDPAIR)
             t)


            ;;-------------------------------------------------------------
            ;; Comment
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_CONTEXT_COMMENT)
             t)


            ;;-------------------------------------------------------------
            ;; Folding White Space
            ;;-------------------------------------------------------------
            ((eql context ISEMAIL_CONTEXT_FWS)
             t)


            ;;-------------------------------------------------------------
            ;; A context we aren't expecting
            ;;-------------------------------------------------------------
            (t     (break "A context we aren't expecting"))

            )
      
      ;; [TODO] remove prior to testing
      (incf element_len)
      (incf element_count)
      (incf context)
      (incf context_prior)
      (setf hyphen_flag t)

      )
    ;; [TODO] remove prior to testing
    (setf checkDNS nil)
    (setf ord      nil)
    (setf end_or_die nil)
    )
  )
 

         ;; (cond ((eql errorlevel E_WARNING) (setf threshold ISEMAIL_THRESHOLD))
         ;;       (t                          (setf threshold (parse-integer errorlevel)))
         ;;       )
