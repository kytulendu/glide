;;
;; THIS SOFTWARE IS SUBJECT TO COPYRIGHT PROTECTION AND IS OFFERED ONLY
;; PURSUANT TO THE 3DFX GLIDE GENERAL PUBLIC LICENSE. THERE IS NO RIGHT
;; TO USE THE GLIDE TRADEMARK WITHOUT PRIOR WRITTEN PERMISSION OF 3DFX
;; INTERACTIVE, INC. A COPY OF THIS LICENSE MAY BE OBTAINED FROM THE
;; DISTRIBUTOR OR BY CONTACTING 3DFX INTERACTIVE INC(info@3dfx.com).
;; THIS PROGRAM IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
;; EXPRESSED OR IMPLIED. SEE THE 3DFX GLIDE GENERAL PUBLIC LICENSE FOR A
;; FULL TEXT OF THE NON-WARRANTY PROVISIONS.
;;
;; USE, DUPLICATION OR DISCLOSURE BY THE GOVERNMENT IS SUBJECT TO
;; RESTRICTIONS AS SET FORTH IN SUBDIVISION (C)(1)(II) OF THE RIGHTS IN
;; TECHNICAL DATA AND COMPUTER SOFTWARE CLAUSE AT DFARS 252.227-7013,
;; AND/OR IN SIMILAR OR SUCCESSOR CLAUSES IN THE FAR, DOD OR NASA FAR
;; SUPPLEMENT. UNPUBLISHED RIGHTS RESERVED UNDER THE COPYRIGHT LAWS OF
;; THE UNITED STATES.
;;
;; COPYRIGHT 3DFX INTERACTIVE, INC. 1999, ALL RIGHTS RESERVED
;;
;; Portion from glide3x/h3/glide3/src/cpudtect.asm
;;

NAME            P6CHK

                .586p
                .model FLAT

_TEXT           SEGMENT

                .code

    PUBLIC _CheckForP6@0
_CheckForP6@0 PROC NEAR
    pushad                              ; save all registers

    ; First, determine whether CPUID instruction is available.
    ; If it's not, then it's a 386 or 486.
    pushfd                              ; push original EFLAGS.
    pop     eax                         ; pop into eax
    mov     ecx, eax                    ; save original EFLAGS in ecx
    xor     eax, 0200000h               ; flip ID bit in EFLAGS
    push    eax                         ; put it back on stack
    popfd                               ; pop into EFLAGS
    pushfd                              ; get EFLAGS back
    pop     eax                         ; into eax
    xor     eax, ecx                    ; check to see if we could toggle ID
    jz      NotPentium                  ; Sorry, not P5 or P6.

    ;
    ; Now determine whether it's an intel P6 CPU.
    ;
    ;;  Is it an Intel CPU?
    xor     eax, eax                    ; eax = 0.
    cpuid                               ; get cpuid
    xor     ebx, 0756e6547h             ; "Genu"
    jnz     NotIntel
    xor     edx, 049656e69h             ; "ineI"
    jnz     NotIntel
    xor     ecx, 06c65746eh             ; "ntel"
    jnz     NotIntel

    ;;  Verifying architecture family
    ;;      eax - type[13:12] family[11:8] model[7:4] revision[3:0]
    ;;      edx - feature bits
    mov     eax, 1
    cpuid                               ; get family/model/stepping

    shr     eax, 8                      ; rid of model & stepping number
    and     eax, 0fh                    ; use only family
    cmp     eax, 6
    jl      CPUisP5                     ; It's a P5
    ;; Else it's a P6

    ; Intel P6 processor.
    ;; feature bits are in edx from the cpuid[1]
CPUisP6:
    ; Make sure it supports Memory Type Range Request registers (bit 12)
    test    edx, 1000h                  ; bit 12 in edx = MTRR support
    jz      CPUisP5
    test    edx, 20h                    ; bit 5 in edx = MSR spport
    jz      CPUisP5
    jmp     DoneCpu

NotPentium:
CPUisP5:
    jmp     noMSR

NotIntel:

    ;; This is a non-Intel processor.

    ;; execute standard feature function

    mov     eax, 1
    cpuid
    mov     ebp, eax                    ; save family/model/stepping

    ;; get the vendor string

    mov     eax, 0
    cpuid

ChkAMD:
    cmp     ebx, 68747541h              ; 'htuA'
    jne     ChkCyrix
    cmp     edx, 69746E65h              ; 'itne'
    jne     ChkCyrix
    cmp     ecx, 444D4163h              ; 'DMAc'
    je      CPUisAMD

ChkCyrix:
    cmp     ebx, 69727943h              ; 'iryC'
    jne     ChkIDT
    cmp     edx, 736E4978h              ; 'snIx'
    jne     ChkIDT
    cmp     ecx, 64616574h              ; 'deat'
    je      CPUisCyrix

ChkIDT:
    cmp     ebx, 746E6543h              ; 'tneC'
    jne     UnknownVendor
    cmp     edx, 48727561h              ; 'Hrua'
    jne     UnknownVendor
    cmp     ecx, 736C7561h              ; 'slua'
    jne     UnknownVendor

CPUisAMD:
    mov     eax, 80010000h              ; vendor = AMD, features = none
    mov     edx, ebp                    ; family/model/stepping information
    and     edx, 00000FFFh              ; extract family/model/stepping
    cmp     edx, 00000588h              ; CXT, Sharptooth, or K7 ?
    jb      AmdBelowK6                  ; nope, definitely no MTRRs
    cmp     edx, 00000600h              ; K7 or better ?
    jb      AmdHasK6MTRR                ; nope, but supports K6 MTRRs
    mov     eax, 1
    jmp     DoneCpu

UnknownVendor:
    ;; execute standard feature function
    mov     eax, 1
    cpuid
    test    edx, 20h                    ; rdmsr/wrmsr support (bit 5)
    jz      noMSR

DoneCpu:
    mov     eax, 1                      ; return 1
    jmp     doneCheck

CPUisCyrix:
CPUisIDT:
AmdBelowK6:
AmdHasK6MTRR:                           ; not support this yet
noMSR:
    mov     eax, 0                      ; return 0

doneCheck:
    popad                               ; restore all registers
    ret
_CheckForP6@0 ENDP

_TEXT   ENDS
        END
