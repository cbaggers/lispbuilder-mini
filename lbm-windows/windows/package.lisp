(in-package #:cl-user)

(defpackage :lbm-windows
  (:use #:cl #:cffi)
  (:nicknames #:win)
  (:documentation "The main package of `lbm-windows'.")
  (:export
   #:WM_CTLCOLORLISTBOX
   #:CREATEDIBSECTION
   #:WM_INITMENUPOPUP
   #:DWEXSTYLE
   #:SHIFTJIS_CHARSET
   #:PAN_PROP_EXPANDED
   #:SM_CYSIZE
   #:MB_DEFBUTTON1
   #:WM_PAINTICON
   #:FW_BLACK
   #:BS_PATTERN8X8
   #:CLIP_EMBEDDED
   #:MERGECOPY
   #:MB_DEFBUTTON2
   #:EN_UPDATE
   #:PAN_WEIGHT_BOOK
   #:MB_DEFBUTTON3
   #:DELETEOBJECT
   #:PAN_STRAIGHT_ARMS_WEDGE
   #:WVR_ALIGNTOP
   #:EM_GETFIRSTVISIBLELINE
   #:WM_PALETTECHANGED
   #:PAN_SERIF_OBTUSE_SQUARE_COVE
   #:SM_CYCURSOR
   #:MSG
   #:BITBLT
   #:FDWQUALITY
   #:WS_EX_TRANSPARENT
   #:WM_INITMENU
   #:SW_MAX
   #:STYLE
   #:HTGROWBOX
   #:BLACK_PEN
   #:BICLRUSED
   #:MK_LBUTTON
   #:WM_QUERYNEWPALETTE
   #:SM_CXSIZE
   #:WM_NCHITTEST
   #:LPTEXT
   #:SM_CXSCREEN
   #:INVALIDATERECT
   #:RGBRESERVED
   #:MK_MBUTTON
   #:MM_ISOTROPIC
   #:WM_NCDESTROY
   #:IDC_UPARROW
   #:PAN_STROKE_RAPID_HORZ
   #:EN_MAXTEXT
   #:MM_HIENGLISH
   #:PAN_LETT_NORMAL_BOXED
   #:SM_CXCURSOR
   #:LPFNWNDPROC
   #:WM_MDIGETACTIVE
   #:EN_CHANGE
   #:WM_MDITILE
   #:NINDEX
   #:OUT_TT_PRECIS
   #:WM_CHILDACTIVATE
   #:ES_WANTRETURN
   #:RELATIVE
   #:CS_SAVEBITS
   #:SM_CXDOUBLECLK
   #:FW_EXTRABOLD
   #:BICOMPRESSION
   #:FF_ROMAN
   #:PAN_STROKE_GRADUAL_HORZ
   #:PAN_LETT_NORMAL_SQUARE
   #:MK_CONTROL
   #:FW_BOLD
   #:FW_DEMIBOLD
   #:WS_EX_NOPARENTNOTIFY
   #:BS_3STATE
   #:MM_ANISOTROPIC
   #:WM_CHANGECBCHAIN
   #:AD_CLOCKWISE
   #:MB_TYPEMASK
   #:SM_CYSCREEN
   #:SIZEZOOMHIDE
   #:LPMODULENAME
   #:SM_CXDLGFRAME
   #:IDC_APPSTARTING
   #:WM_MEASUREITEM
   #:EM_GETLINE
   #:PAN_PROP_MODERN
   #:SM_DBCSENABLED
   #:EM_LIMITTEXT
   #:CREATESOLIDBRUSH
   #:SM_CYMIN
   #:SM_CMOUSEBUTTONS
   #:WM_PAINTCLIPBOARD
   #:SM_DEBUG
   #:LPCURSORNAME
   #:FILLRECT
   #:WM_CLOSE
   #:SYSTEM_FIXED_FONT
   #:NIDEVENT
   #:MM_TEXT
   #:CREATECOMPATIBLEDC
   #:STM_GETICON
   #:DRAFT_QUALITY
   #:WM_DROPFILES
   #:BS_GROUPBOX
   #:ES_LOWERCASE
   #:EN_SETFOCUS
   #:WM_NCMOUSEMOVE
   #:PAN_WEIGHT_DEMI
   #:PAN_LETT_OBLIQUE_CONTACT
   #:OUT_DEFAULT_PRECIS
   #:PAN_SERIF_OBTUSE_COVE
   #:WS_DLGFRAME
   #:WM_EXITMENULOOP
   #:OUT_RASTER_PRECIS
   #:MB_RETRYCANCEL
   #:CREATESOLIDBRUSH
   #:WS_MAXIMIZEBOX
   #:WM_DEADCHAR
   #:SM_CXMIN
   #:WM_KEYFIRST
   #:CBCOUNT
   #:HTTRANSPARENT
   #:CLIP_STROKE_PRECIS
   #:HOLLOW_BRUSH
   #:HTSYSMENU
   #:GETSYSTEMMETRICS
   #:WM_MDINEXT
   #:MB_YESNO
   #:SM_RESERVED1
   #:PAN_XHEIGHT_CONSTANT_STD
   #:FW_HEAVY
   #:EM_SETWORDBREAKPROC
   #:BS_OWNERDRAW
   #:PAN_MIDLINE_CONSTANT_POINTED
   #:MESSAGEBOX
   #:PTMAXTRACKSIZE
   #:GETSTOCKOBJECT
   #:DWDATA
   #:SW_SHOWMINNOACTIVE
   #:EXTTEXTOUT
   #:SIZE_RESTORED
   #:PAN_XHEIGHT_DUCKING_STD
   #:FW_REGULAR
   #:HTERROR
   #:PAN_LETT_NORMAL_OFF_CENTER
   #:WHITE_BRUSH
   #:EN_ERRSPACE
   #:SS_RIGHT
   #:WM_ENDSESSION
   #:LPARAM
   #:NYSRC
   #:WM_MDIACTIVATE
   #:WM_PENWINFIRST
   #:ES_RIGHT
   #:WM_SYNCPAINT
   #:PAN_SERIF_SQUARE_COVE
   #:PAN_WEIGHT_BOLD
   #:NXSRC
   #:PS_JOIN_BEVEL
   #:DEVICE_DEFAULT_FONT
   #:MB_APPLMODAL
   #:PAN_PROP_CONDENSED
   #:MN_GETHMENU
   #:BISIZE
   #:WM_MDISETMENU
   #:HGDIOBJ
   #:UIDEVENT
   #:HWNDPARENT
   #:GETCLIENTRECT
   #:PAN_MIDLINE_LOW_SERIFED
   #:PTMAXPOSITION
   #:WM_COPY
   #:NWIDTH
   #:FILLRECT
   #:FF_DONTCARE
   #:BN_CLICKED
   #:WS_DISABLED
   #:SS_GRAYFRAME
   #:PAN_XHEIGHT_CONSTANT_LARGE
   #:NYDEST
   #:SS_ICON
   #:SMTO_ABORTIFHUNG
   #:PAN_MIDLINE_STANDARD_TRIMMED
   #:BM_SETSTYLE
   #:CS_HREDRAW
   #:ETO_CLIPPED
   #:PS_DOT
   #:PWR_FAIL
   #:EM_GETLINECOUNT
   #:EM_UNDO
   #:PWR_OK
   #:NXDEST
   #:WM_SYSKEYUP
   #:WM_DEVMODECHANGE
   #:WM_TIMER
   #:SS_LEFTNOWORDWRAP
   #:WM_CUT
   #:GETMESSAGE
   #:PS_ENDCAP_FLAT
   #:EM_GETSEL
   #:PAN_LETT_NORMAL_CONTACT
   #:GM_LAST
   #:FLAGS
   #:ANSI_CHARSET
   #:REGISTERCLASS
   #:PWR_SUSPENDRESUME
   #:ABSOLUTE
   #:ICON_SMALL
   #:EM_GETTHUMB
   #:PAN_LETT_OBLIQUE_BOXED
   #:FIXED_PITCH
   #:ANSI_FIXED_FONT
   #:PAN_STRAIGHT_ARMS_HORZ
   #:EN_KILLFOCUS
   #:IDC_SIZENESW
   #:WM_NCRBUTTONUP
   #:LPPAINT
   #:IDC_IBEAM
   #:SYMBOL_CHARSET
   #:ES_OEMCONVERT
   #:MB_SETFOREGROUND
   #:FW_NORMAL
   #:SS_WHITERECT
   #:EM_SETREADONLY
   #:UPDATEWINDOW
   #:OEM_FIXED_FONT
   #:WM_MDIMAXIMIZE
   #:EM_SETRECTNP
   #:WM_CANCELJOURNAL
   #:HTMENU
   #:CBWNDEXTRA
   #:WM_NULL
   #:WM_POWER
   #:GETMESSAGE
   #:WM_SETFONT
   #:MB_MODEMASK
   #:IUSAGE
   #:WVR_ALIGNLEFT
   #:PAN_PROPORTION_INDEX
   #:PAN_MIDLINE_CONSTANT_TRIMMED
   #:HS_FDIAGONAL
   #:CS_BYTEALIGNWINDOW
   #:WM_CTLCOLORSCROLLBAR
   #:WM_MDIRESTORE
   #:WM_DRAWITEM
   #:PAN_LETTERFORM_INDEX
   #:HTTOPRIGHT
   #:BS_PUSHBUTTON
   #:FDWOUTPUTPRECISION
   #:LPSZCLASSNAME
   #:PAN_CONTRAST_MEDIUM_LOW
   #:PAN_WEIGHT_HEAVY
   #:FINCUPDATE
   #:KILLTIMER
   #:FDWCHARSET
   #:PBMI
   #:GETWINDOWTEXT
   #:IDC_ARROW
   #:WS_POPUPWINDOW
   #:PS_ENDCAP_ROUND
   #:PAN_WEIGHT_NORD
   #:SM_CYHSCROLL
   #:WM_NEXTDLGCTL
   #:SW_SHOWNA
   #:PAN_WEIGHT_VERY_LIGHT
   #:SW_MAXIMIZE
   #:RGBGREEN
   #:FUOPTIONS
   #:PAN_PROP_OLD_STYLE
   #:BS_LEFTTEXT
   #:HTSIZE
   #:WM_COMMAND
   #:WM_ICONERASEBKGND
   #:WS_MAXIMIZE
   #:POSTQUITMESSAGE
   #:LPDX
   #:PAN_WEIGHT_LIGHT
   #:WM_ENTERSIZEMOVE
   #:SRCAND
   #:PS_NULL
   #:LTGRAY_BRUSH
   #:PAN_LETT_OBLIQUE_WEIGHTED
   #:DEFAULT_CHARSET
   #:MOVETOEX
   #:WS_VISIBLE
   #:EM_SETSEL
   #:FW_EXTRALIGHT
   #:WM_CHARTOITEM
   #:PS_ALTERNATE
   #:EM_CANUNDO
   #:ELF_VENDOR_SIZE
   #:DEFAULT_PITCH
   #:BITMAPINFOHEADER
   #:BS_PUSHBOX
   #:WVR_VALIDRECTS
   #:LPWINDOWNAME
   #:GETWINDOWTEXT
   #:BMIHEADER
   #:PAN_WEIGHT_THIN
   #:SM_RESERVED2
   #:CS_PARENTDC
   #:SENDMESSAGE
   #:SM_CYKANJIWINDOW
   #:NULL_BRUSH
   #:PS_STYLE_MASK
   #:WM_MBUTTONUP
   #:GETSYSTEMMETRICS
   #:PAN_XHEIGHT_DUCKING_SMALL
   #:DEFWINDOWPROC
   #:NORIENTATION
   #:HDCSRC
   #:SW_SHOWMINIMIZED
   #:DSTINVERT
   #:NULL_PEN
   #:STM_MSGMAX
   #:HS_CROSS
   #:DELETEOBJECT
   #:HS_DIAGCROSS
   #:CREATEDIBSECTION
   #:PT
   #:MA_ACTIVATEANDEAT
   #:PC_RESERVED
   #:PTMINTRACKSIZE
   #:WM_CANCELMODE
   #:PAN_MIDLINE_HIGH_SERIFED
   #:MB_ICONASTERISK
   #:EXTTEXTOUT
   #:PROOF_QUALITY
   #:SW_NORMAL
   #:WS_TILED
   #:FF_MODERN
   #:SS_NOPREFIX
   #:HTTOP
   #:GETCLIENTRECT
   #:CS_VREDRAW
   #:MB_DEFMASK
   #:MM_LOMETRIC
   #:EM_LINEINDEX
   #:WM_CLEAR
   #:SS_BLACKRECT
   #:LPPARAM
   #:EM_SETHANDLE
   #:SIZE_MAXSHOW
   #:WM_PASTE
   #:SS_USERITEM
   #:POSTQUITMESSAGE
   #:DISPATCHMESSAGE
   #:SM_CXHSCROLL
   #:WM_VSCROLL
   #:KILLTIMER
   #:PAN_LETT_NORMAL_ROUNDED
   #:PAN_MIDLINE_INDEX
   #:GM_ADVANCED
   #:LPSZMENUNAME
   #:WS_CLIPCHILDREN
   #:EM_GETRECT
   #:SIZEICONIC
   #:BIBITCOUNT
   #:LPCLASSNAME
   #:WS_POPUP
   #:WM_NCACTIVATE
   #:LST
   #:WM_MBUTTONDBLCLK
   #:PWR_CRITICALRESUME
   #:NEXITCODE
   #:FDWPITCHANDFAMILY
   #:PAN_PROP_VERY_CONDENSED
   #:SM_CYCAPTION
   #:CREATEFONT
   #:CLIP_LH_ANGLES
   #:CLIP_CHARACTER_PRECIS
   #:WB_ISDELIMITER
   #:WM_GETFONT
   #:MB_ICONINFORMATION
   #:BS_USERBUTTON
   #:BITBLT
   #:WM_COMPAREITEM
   #:MM_LOENGLISH
   #:WM_KILLFOCUS
   #:BN_PAINT
   #:PAN_PROP_VERY_EXPANDED
   #:RASTER_FONTTYPE
   #:MM_MIN
   #:BS_AUTO3STATE
   #:IDC_CROSS
   #:WM_NCPAINT
   #:LPDATA
   #:CBDATA
   #:MB_ICONMASK
   #:WA_CLICKACTIVE
   #:WM_LBUTTONDBLCLK
   #:EM_GETWORDBREAKPROC
   #:PAN_SERIF_EXAGGERATED
   #:WM_VKEYTOITEM
   #:PC_NOCOLLAPSE
   #:WS_ICONIC
   #:WM_HSCROLL
   #:WM_NCCREATE
   #:WM_USER
   #:WM_QUIT
   #:SM_CYMENU
   #:lbm-WINDOWS
   #:MB_OK
   #:FDWCLIPPRECISION
   #:BIWIDTH
   #:ETO_OPAQUE
   #:WM_GETMINMAXINFO
   #:MB_DEFAULT_DESKTOP_ONLY
   #:CS_OWNDC
   #:WM_GETDLGCODE
   #:IDC_SIZEALL
   #:UPDATEWINDOW
   #:PS_DASHDOTDOT
   #:PT_CLOSEFIGURE
   #:SM_SHOWSOUNDS
   #:WM_KEYUP
   #:SS_GRAYRECT
   #:MM_HIMETRIC
   #:PAN_BENT_ARMS_VERT
   #:SM_MENUDROPALIGNMENT
   #:PAN_ANY
   #:BS_DIBPATTERNPT
   #:WM_CTLCOLOREDIT
   #:SETTIMER
   #:BREPAINT
   #:BS_MONOPATTERN
   #:GM_COMPATIBLE
   #:ES_READONLY
   #:SRCERASE
   #:RGBRED
   #:WM_CTLCOLORDLG
   #:MB_ICONEXCLAMATION
   #:CLR_INVALID
   #:PC_EXPLICIT
   #:ENDPAINT
   #:WS_OVERLAPPED
   #:PAN_WEIGHT_MEDIUM
   #:MESSAGE
   #:WM_GETTEXT
   #:OUT_OUTLINE_PRECIS
   #:STOCK_LAST
   #:SW_SHOWDEFAULT
   #:WM_NCRBUTTONDBLCLK
   #:WM_GETTEXTLENGTH
   #:PAN_XHEIGHT_CONSTANT_SMALL
   #:WPARAM
   #:PAN_CONTRAST_VERY_HIGH
   #:CS_BYTEALIGNCLIENT
   #:ICON_BIG
   #:OPAQUE
   #:WM_LBUTTONUP
   #:PAN_WEIGHT_INDEX
   #:LOADCURSOR
   #:GETSTOCKOBJECT
   #:BS_HATCHED
   #:LPRECT
   #:WS_GROUP
   #:WS_SYSMENU
   #:CHINESEBIG5_CHARSET
   #:DEFWINDOWPROC
   #:GRAY_BRUSH
   #:SM_SWAPBUTTON
   #:ES_PASSWORD
   #:PAN_NO_FIT
   #:SW_SHOWNOACTIVATE
   #:WS_MINIMIZEBOX
   #:EM_GETHANDLE
   #:WM_MENUCHAR
   #:DEVICE_FONTTYPE
   #:BKMODE_LAST
   #:WM_GETHOTKEY
   #:WM_MOUSEACTIVATE
   #:HS_VERTICAL
   #:HTMAXBUTTON
   #:HWND
   #:FORMATT
   #:HTBOTTOMRIGHT
   #:DIB_RGB_COLORS
   #:IDC_SIZENWSE
   #:PAN_SERIF_OBTUSE_SANS
   #:WM_ENTERIDLE
   #:PAN_MIDLINE_CONSTANT_SERIFED
   #:HTSIZEFIRST
   #:EM_SCROLLCARET
   #:SM_CYICONSPACING
   #:BN_DOUBLECLICKED
   #:WM_ENTERMENULOOP
   #:HDCDEST
   #:PAN_XHEIGHT_DUCKING_LARGE
   #:NYEND
   #:WM_RBUTTONDOWN
   #:WM_TIMECHANGE
   #:PAN_SERIF_PERP_SANS
   #:BS_PATTERN
   #:FW_ULTRABOLD
   #:HBR
   #:FRESTORE
   #:PAN_BENT_ARMS_SINGLE_SERIF
   #:BN_HILITE
   #:PPVBITS
   #:NXEND
   #:WM_HSCROLLCLIPBOARD
   #:WM_WINDOWPOSCHANGED
   #:FDWITALIC
   #:DISPATCHMESSAGE
   #:WM_RENDERALLFORMATS
   #:WM_RBUTTONUP
   #:WM_SYSCOLORCHANGE
   #:SMTO_BLOCK
   #:NCCALCSIZE_PARAMS
   #:PAN_MIDLINE_HIGH_POINTED
   #:PT_LINETO
   #:MM_MAX_FIXEDSCALE
   #:OUT_PS_ONLY_PRECIS
   #:CS_NOCLOSE
   #:SM_CYFRAME
   #:SM_CXMINTRACK
   #:PAN_CONTRAST_HIGH
   #:WM_MBUTTONDOWN
   #:EM_SETRECT
   #:WM_NCMBUTTONDBLCLK
   #:HTCLIENT
   #:IDC_SIZE
   #:WM_NCLBUTTONDBLCLK
   #:WHITENESS
   #:MB_ICONSTOP
   #:PS_ENDCAP_MASK
   #:WM_SETTEXT
   #:MK_SHIFT
   #:WS_CLIPSIBLINGS
   #:WS_TABSTOP
   #:SM_RESERVED3
   #:HTHSCROLL
   #:WM_DRAWCLIPBOARD
   #:EN_VSCROLL
   #:WS_OVERLAPPEDWINDOW
   #:FF_SWISS
   #:SM_CXFRAME
   #:SM_CXICONSPACING
   #:BS_AUTORADIOBUTTON
   #:HOBJECT
   #:BICLRIMPORTANT
   #:LPCAPTION
   #:PAN_MIDLINE_STANDARD_POINTED
   #:HTCAPTION
   #:WM_SIZE
   #:RECT
   #:LPWNDCLASS
   #:HTBOTTOMLEFT
   #:WM_SYSKEYDOWN
   #:SS_BLACKFRAME
   #:PAN_STROKEVARIATION_INDEX
   #:PAN_LETT_OBLIQUE_SQUARE
   #:IDC_SIZEWE
   #:WM_SYSCHAR
   #:PS_COSMETIC
   #:WM_WINDOWPOSCHANGING
   #:SW_FORCEMINIMIZE
   #:LPSZFACE
   #:BS_RADIOBUTTON
   #:PT_MOVETO
   #:HTBOTTOM
   #:DIB_PAL_COLORS
   #:EM_GETPASSWORDCHAR
   #:SETMAPMODE
   #:PAN_LETT_OBLIQUE_ROUNDED
   #:SIZENORMAL
   #:BS_DEFPUSHBUTTON
   #:OUT_STRING_PRECIS
   #:WM_MOVE
   #:TRUETYPE_FONTTYPE
   #:SIZE_MAXHIDE
   #:WM_SETHOTKEY
   #:WM_COMMNOTIFY
   #:EN_HSCROLL
   #:LEFT
   #:POINT
   #:NOTSRCERASE
   #:PS_JOIN_MITER
   #:FW_MEDIUM
   #:DEFAULT_PALETTE
   #:MM_MAX
   #:CREATEWINDOWEX
   #:LINETO
   #:WM_NCLBUTTONUP
   #:WM_PENWINLAST
   #:PAN_SERIF_NORMAL_SANS
   #:NOTSRCCOPY
   #:MINMAXINFO
   #:BIYPELSPERMETER
   #:MB_YESNOCANCEL
   #:PS_USERSTYLE
   #:WM_SHOWWINDOW
   #:AD_COUNTERCLOCKWISE
   #:BIXPELSPERMETER
   #:WM_KEYDOWN
   #:BI_BITFIELDS
   #:HDC
   #:PAN_SERIF_THIN
   #:PS_DASHDOT
   #:PS_TYPE_MASK
   #:PAN_STRAIGHT_ARMS_VERT
   #:BI_JPEG
   #:WS_CHILD
   #:WM_QUERYOPEN
   #:WM_CTLCOLORMSGBOX
   #:SW_HIDE
   #:BI_PNG
   #:FF_SCRIPT
   #:NESCAPEMENT
   #:OEM_CHARSET
   #:SELECTOBJECT
   #:SM_CYMINTRACK
   #:BS_NULL
   #:IDC_NO
   #:PAN_CONTRAST_LOW
   #:INVALIDATERECT
   #:BS_AUTOCHECKBOX
   #:CLIP_TT_ALWAYS
   #:PAN_ARMSTYLE_INDEX
   #:REGISTERCLASS
   #:VARIABLE_PITCH
   #:BS_DIBPATTERN8X8
   #:NMAXCOUNT
   #:WM_MDICASCADE
   #:DEFAULT_QUALITY
   #:SETBKCOLOR
   #:WM_MDIDESTROY
   #:EM_SCROLL
   #:PAN_MIDLINE_STANDARD_SERIFED
   #:SETMAPMODE
   #:PAN_FAMILYTYPE_INDEX
   #:ES_AUTOVSCROLL
   #:PS_JOIN_MASK
   #:HSECTION
   #:PS_SOLID
   #:CS_DBLCLKS
   #:BIPLANES
   #:WM_RENDERFORMAT
   #:PAN_FAMILY_TEXT_DISPLAY
   #:SM_CXHTHUMB
   #:BLACK_BRUSH
   #:HTTOPLEFT
   #:SS_LEFT
   #:EM_FMTLINES
   #:BS_SOLID
   #:RGRC
   #:Y
   #:LPPOINT
   #:SRCCOPY
   #:WM_SETFOCUS
   #:X
   #:PAINTSTRUCT
   #:WM_CREATE
   #:HINSTANCE
   #:CREATECOMPATIBLEDC
   #:ANSI_VAR_FONT
   #:PAN_FAMILY_DECORATIVE
   #:WS_EX_ACCEPTFILES
   #:PAN_CONTRAST_INDEX
   #:PAN_MIDLINE_LOW_POINTED
   #:ENDPAINT
   #:WM_LBUTTONDOWN
   #:RGBQUAD
   #:WM_UNDO
   #:WM_PAINT
   #:WVR_ALIGNRIGHT
   #:PAN_SERIF_TRIANGLE
   #:BN_DISABLE
   #:S
   #:HWNDINSERTAFTER
   #:PS_ENDCAP_SQUARE
   #:WM_DELETEITEM
   #:BMICOLORS
   #:WS_BORDER
   #:PAN_CONTRAST_MEDIUM_HIGH
   #:MOVEWINDOW
   #:DWSTYLE
   #:P
   #:FW_THIN
   #:WVR_HREDRAW
   #:SM_MOUSEPRESENT
   #:FDWUNDERLINE
   #:MM_TWIPS
   #:BS_TYPEMASK
   #:SIZE_MINIMIZED
   #:LPRC
   #:PATCOPY
   #:WM_SPOOLERSTATUS
   #:GETMODULEHANDLE
   #:SW_SHOW
   #:SHOWWINDOW
   #:SM_CYVTHUMB
   #:GETMODULEHANDLE
   #:MA_ACTIVATE
   #:BOTTOM
   #:LPTIMERFUNC
   #:BLACKNESS
   #:TEMP
   #:WM_ACTIVATE
   #:PAN_CONTRAST_VERY_LOW
   #:PAN_STROKE_INSTANT_VERT
   #:ELF_VERSION
   #:DWROP
   #:WM_MDICREATE
   #:HS_HORIZONTAL
   #:SIZE_MAXIMIZED
   #:PAN_STROKE_RAPID_VERT
   #:ELF_CULTURE_LATIN
   #:BS_CHECKBOX
   #:SW_SHOWMAXIMIZED
   #:WM_MDIICONARRANGE
   #:WMSGFILTERMAX
   #:STM_SETICON
   #:PAN_CONTRAST_NONE
   #:PAN_LETT_NORMAL_WEIGHTED
   #:WM_INITDIALOG
   #:BM_SETSTATE
   #:SRCPAINT
   #:PAN_SERIF_BONE
   #:DWOFFSET
   #:WA_ACTIVE
   #:WM_COMPACTING
   #:OUT_TT_ONLY_PRECIS
   #:EM_REPLACESEL
   #:SMTO_NORMAL
   #:EM_SETTABSTOPS
   #:BIHEIGHT
   #:WM_NCCALCSIZE
   #:MESSAGEBOX
   #:WM_FONTCHANGE
   #:HTREDUCE
   #:SM_CYFULLSCREEN
   #:PAN_WEIGHT_BLACK
   #:WINDOWPOS
   #:HTSIZELAST
   #:PAN_SERIF_FLARED
   #:WVR_VREDRAW
   #:TRANSLATEMESSAGE
   #:HMENU
   #:WM_ERASEBKGND
   #:PATINVERT
   #:SM_RESERVED4
   #:WM_NCRBUTTONDOWN
   #:PAN_FAMILY_SCRIPT
   #:WS_VSCROLL
   #:LINETO
   #:WHITE_PEN
   #:WM_RBUTTONDBLCLK
   #:PAN_BENT_ARMS_HORZ
   #:PTMAXSIZE
   #:WVR_REDRAW
   #:PAN_LETT_NORMAL_FLATTENED
   #:COPYDATASTRUCT
   #:BS_INDEXED
   #:WS_EX_TOPMOST
   #:PAN_MIDLINE_HIGH_TRIMMED
   #:WM_DESTROY
   #:WM_CTLCOLORBTN
   #:LPMSG
   #:WS_CHILDWINDOW
   #:FW_ULTRALIGHT
   #:FNOBJECT
   #:UTYPE
   #:LPPOS
   #:BEGINPAINT
   #:WB_LEFT
   #:WM_SETREDRAW
   #:WM_ACTIVATEAPP
   #:BM_GETCHECK
   #:CLIP_MASK
   #:BS_DIBPATTERN
   #:WM_PALETTEISCHANGING
   #:CS_CLASSDC
   #:MB_MISCMASK
   #:TRANSLATEMESSAGE
   #:WM_DESTROYCLIPBOARD
   #:SS_CENTER
   #:CLIP_DEFAULT_PRECIS
   #:WS_CAPTION
   #:SETTIMER
   #:PAN_BENT_ARMS_WEDGE
   #:WM_NCLBUTTONDOWN
   #:SW_MINIMIZE
   #:ES_AUTOHSCROLL
   #:MB_SYSTEMMODAL
   #:WM_SYSDEADCHAR
   #:WS_MINIMIZE
   #:WM_KEYLAST
   #:WM_NCMBUTTONDOWN
   #:ES_CENTER
   #:OUT_STROKE_PRECIS
   #:LPSTRING
   #:EM_EMPTYUNDOBUFFER
   #:HTVSCROLL
   #:BI_RLE8
   #:WS_HSCROLL
   #:WM_NCMBUTTONUP
   #:PAN_STROKE_GRADUAL_DIAG
   #:PAN_SERIF_ROUNDED
   #:PS_INSIDEFRAME
   #:MK_RBUTTON
   #:WS_EX_DLGMODALFRAME
   #:PAN_STRAIGHT_ARMS_SINGLE_SERIF
   #:EM_SETMODIFY
   #:BI_RLE4
   #:IDC_SIZENS
   #:FW_LIGHT
   #:NHEIGHT
   #:HTZOOM
   #:NCMDSHOW
   #:PAN_BENT_ARMS_DOUBLE_SERIF
   #:OUT_SCREEN_OUTLINE_PRECIS
   #:PWR_SUSPENDREQUEST
   #:SM_CYVSCROLL
   #:LOADCURSOR
   #:MOVETOEX
   #:HANGUL_CHARSET
   #:PANOSE_COUNT
   #:HTBORDER
   #:SHOWWINDOW
   #:PTRESERVED
   #:WM_SYSCOMMAND
   #:PAN_STROKE_GRADUAL_VERT
   #:SW_SHOWNORMAL
   #:FNWEIGHT
   #:PATPAINT
   #:PAN_FAMILY_PICTORIAL
   #:HTNOWHERE
   #:WA_INACTIVE
   #:WM_EXITSIZEMOVE
   #:OUT_CHARACTER_PRECIS
   #:HTLEFT
   #:ES_UPPERCASE
   #:SS_WHITEFRAME
   #:MB_OKCANCEL
   #:RGBBLUE
   #:HS_BDIAGONAL
   #:WM_QUEUESYNC
   #:OUT_DEVICE_PRECIS
   #:HICON
   #:HTRIGHT
   #:WM_MOUSEFIRST
   #:SS_SIMPLE
   #:PAN_LETT_OBLIQUE_FLATTENED
   #:SM_CXFULLSCREEN
   #:FERASE
   #:IDC_ICON
   #:WM_ENABLE
   #:MB_ABORTRETRYIGNORE
   #:TRANSPARENT
   #:BEGINPAINT
   #:SRCINVERT
   #:BITMAPINFO
   #:WS_THICKFRAME
   #:WM_HOTKEY
   #:DKGRAY_BRUSH
   #:WM_SIZECLIPBOARD
   #:GB2312_CHARSET
   #:WM_WININICHANGE
   #:WMSGFILTERMIN
   #:WM_MOUSELAST
   #:TOP
   #:CY
   #:CREATEWINDOWEX
   #:WM_VSCROLLCLIPBOARD
   #:SELECTOBJECT
   #:CX
   #:WM_QUERYENDSESSION
   #:MB_TASKMODAL
   #:FW_SEMIBOLD
   #:CBCLSEXTRA
   #:WS_SIZEBOX
   #:PS_GEOMETRIC
   #:RCPAINT
   #:PT_BEZIERTO
   #:WM_ASKCBFORMATNAME
   #:PAN_CONTRAST_MEDIUM
   #:BN_UNHILITE
   #:FF_DECORATIVE
   #:WM_SETCURSOR
   #:WB_RIGHT
   #:PAN_CULTURE_LATIN
   #:WM_PARENTNOTIFY
   #:EM_SETPASSWORDCHAR
   #:PS_DASH
   #:PAN_STROKE_GRADUAL_TRAN
   #:BM_GETSTATE
   #:WM_MOUSEMOVE
   #:RIGHT
   #:PAN_SERIF_COVE
   #:SIZEFULLSCREEN
   #:WM_MENUSELECT
   #:BISIZEIMAGE
   #:PAN_MIDLINE_LOW_TRIMMED
   #:HBRBACKGROUND
   #:BS_HOLLOW
   #:EM_LINESCROLL
   #:WVR_ALIGNBOTTOM
   #:EM_LINELENGTH
   #:SM_CYDOUBLECLK
   #:WNDCLASS
   #:HTMINBUTTON
   #:HCURSOR
   #:WM_COPYDATA
   #:PS_JOIN_ROUND
   #:EM_LINEFROMCHAR
   #:SIZEZOOMSHOW
   #:FDWSTRIKEOUT
   #:SM_CMETRICS
   #:SENDMESSAGE
   #:SM_CYDLGFRAME
   #:SM_CXICON
   #:PAN_SERIFSTYLE_INDEX
   #:PAN_STRAIGHT_ARMS_DOUBLE_SERIF
   #:ES_LEFT
   #:PAN_LETT_OBLIQUE_OFF_CENTER
   #:WM_MDIREFRESHMENU
   #:PAN_SERIF_SQUARE
   #:FW_DONTCARE
   #:PAN_PROP_EVEN_WIDTH
   #:MB_ICONHAND
   #:MOVEWINDOW
   #:MB_ICONQUESTION
   #:PAN_XHEIGHT_INDEX
   #:SM_CXVSCROLL
   #:SM_CYBORDER
   #:ES_MULTILINE
   #:BI_RGB
   #:FNMAPMODE
   #:MA_NOACTIVATE
   #:WM_CTLCOLORSTATIC
   #:MB_NOFOCUS
   #:SW_RESTORE
   #:BM_SETCHECK
   #:PAN_PROP_MONOSPACED
   #:SM_CYICON
   #:HANGEUL_CHARSET
   #:WM_CHAR
   #:UELAPSE
   #:WM_QUERYDRAGICON
   #:MA_NOACTIVATEANDEAT
   #:EM_GETMODIFY
   #:IDC_WAIT
   #:SM_PENWINDOWS
   #:MERGEPAINT
   #:ES_NOHIDESEL
   #:CREATEFONT
   #:CRCOLOR
   #:CS_GLOBALCLASS
   #:SETBKCOLOR
   #:SYSTEM_FONT
   #:SM_CXBORDER

   #:GET-DC
   #:DESTROY-WINDOW

   #:VALIDATE-RECT
   #:CREATESTRUCT
   #:SET-PIXEL-FORMAT
   #:CHOOSE-PIXEL-FORMAT
   #:PIXELFORMATDESCRIPTOR

   #:PFD_DRAW_TO_WINDOW
   #:PFD_SUPPORT_OPENGL
   #:PFD_DOUBLEBUFFER
   #:PFD_TYPE_RGBA
   #:PFD_MAIN_PLANE
   #:GET-WINDOW-RECT
   ))
