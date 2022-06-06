{------------------------------------------------------------------------------}
{                                                                              }
{  Print Preview Components Suit                                               }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{  TPrintPreview v4.77                                                         }
{  TPaperPreview v1.03                                                         }
{  TThumbnailPreview v1.02                                                     }
{                                                                              }
{  Special thanks to:                                                          }
{    Rinaldo Andrea (Italy)         <laser@nuovacs.it>                         }
{    Jens Christian Fogtmann        <jefo@post2.tele.dk>                       }
{    Damian Tarnawsky               <tarnawsky@ali.com.au>                     }
{    Bill Miller                    <w2m@netheaven.com>                        }
{    Wen Shihong                    <wenshihong@justep.com>                    }
{    Peter Hedlund                  <peter@peterandlinda.com>                  }
{    Pavel Zidek (Czech)            <delphi@kopr.cz>            (SaveZoomPos)  }
{    Roy M Klever                   <roy.magne@os.ino.no>       //rmk          }
{    Paul Van Gundy                 <b06pxv@FDS.com>            //pvg          }
{    Hubert "Johnny_Bit" Kowalski   <johnnybit@poczta.onet.pl>                 }
{    Tomas Koutny                   <rawos@rawos.com>                          }
{    Patrizio Zelotti               <pzelotti@libero.it>                       }
{    Bria Dorin                     <briadorin@yahoo.com>                      }
{    Arpad Toth                     <atsoft@atsoftware.czweb.org>              }
{    Janet Agney                    <janet.agney@vaisala.com>                  }
{    MeW                            <marco@wobben.com>                         }
{    Mixy                           N/A                         //Mixy         }
{    Miguel Gastelumendi            <mgd@satelier.com.br>                      }
{    akeix                          N/A                                        }
{    DwrCymru                       N/A                                        }
{    John Hodgson                   <JohnHodgson@qmap.co.uk>                   }
{                                                                              }
{------------------------------------------------------------------------------}

// Use ZLib Compression and Decompression for Streaming
// Get the library at http://www.gzip.org/zlib
{.$DEFINE ZLIB}

// If you need transparent image printing, define IMAGE_TRANSPARENCY
// Transparency on printers is not guaranteed. Instead, combine images
// as needed, and then draw the final image to the printer.
{.$DEFINE IMAGE_TRANSPARENCY}

{$I DELPHIAREA.INC}

unit Preview;

interface

uses
  system.types, Windows, WinSpool, Messages, Classes, Graphics, Controls, SysUtils, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, Printers;

const
  crHand = 10;
  crGrab = 11; //pvg

type

  EInvalidPreviewData = class(Exception);
  EMissingPDFLibrary = class(Exception);

  { TMetafileList }

  TMetafileChangeEvent = procedure(Sender: TObject; Index: Integer) of object;

  TMetafileList = class(TObject)
  private
    FRecords: TList;
    FDataStream: TStream;
    FLoadedIndex: Integer;
    FLoadedMetafile: TMetafile;
    FUseTempFile: Boolean;
    FTempFile: String;
    FOnChange: TNotifyEvent;
    FOnCurrentChange: TMetafileChangeEvent;
    function GetCount: Integer;
    function GetItems(Index: Integer): TMetafile;
    procedure SetLoadedIndex(Value: Integer);
    procedure SetUseTempFile(Value: Boolean);
    procedure MetafileChanged(Sender: TObject);
  protected
    function CreateMetafileStream: TStream;
    procedure Cleanup;
    property LoadedIndex: Integer read FLoadedIndex write SetLoadedIndex;
    property LoadedMetafile: TMetafile read FLoadedMetafile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AMetafile: TMetafile): Integer;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMetafile read GetItems; default;
    property UseTempFile: Boolean read FUseTempFile write SetUseTempFile;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCurrentChange: TMetafileChangeEvent read FOnCurrentChange write FOnCurrentChange;
  end;

  { TPaperPreview }

  TPaperPaintEvent = procedure(Sender: TObject; Canvas: TCanvas;
    const PageRect: TRect) of object;

  TPaperPreview = class(TCustomControl)
  private
    FPaperColor: TColor;
    FBorderColor: TColor;
    FBorderSize: TBorderWidth;
    FShadowColor: TColor;
    FShadowSize: TBorderWidth;
    FOnResize: TNotifyEvent;
    FOnPaint: TPaperPaintEvent;
    FOffScreen: TBitmap;
    FOffScreenValid: Boolean;
    FOffScreenPrepared: Boolean;
    FOffScreenDrawn: Boolean;
    procedure SetPaperWidth(Value: Integer);
    function GetPaperWidth: Integer;
    procedure SetPaperHeight(Value: Integer);
    function GetPaperHeight: Integer;
    procedure SetPaperColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderSize(Value: TBorderWidth);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowSize(Value: TBorderWidth);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure PrepareOffScreen; virtual;
    procedure GetPageRect(var Rect: TRect); virtual;
    function ActualSize(Value: Integer): Integer;
    function LogicalSize(Value: Integer): Integer;
    property OffScreenValid: Boolean read FOffScreenValid;
    property OffScreenPrepared: Boolean read FOffScreenPrepared;
    property OffScreenIsDirty: Boolean read FOffScreenDrawn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
  published
    property Align;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderSize: TBorderWidth read FBorderSize write SetBorderSize default 1;
    property Color;
    property Cursor;
    property DragCursor;
    property DragMode;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property PaperColor: TColor read FPaperColor write SetPaperColor default clWhite;
    property PaperWidth: Integer read GetPaperWidth write SetPaperWidth;
    property PaperHeight: Integer read GetPaperHeight write SetPaperHeight;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property ShadowSize: TBorderWidth read FShadowSize write SetShadowSize default 3;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnPaint: TPaperPaintEvent read FOnPaint write FOnPaint;
  end;

  { TPaperPreviewOptions }

  TPaperPreviewOptions = class(TPersistent)
  private
    FPaperColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: TBorderWidth;
    FShadowColor: TColor;
    FShadowWidth: TBorderWidth;
    FCursor: TCursor;
    FDragCursor: TCursor;
    FGrabCursor: TCursor; //pvg
    FPopupMenu: TPopupMenu;
    FOnChange: TNotifyEvent;
    procedure SetPaperColor(Value: TColor);
    procedure SetBorderColor(Value: TColor);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowWidth(Value: TBorderWidth);
    procedure SetCursor(Value: TCursor);
    procedure SetDragCursor(Value: TCursor);
    procedure SetGrabCursor(Value: TCursor); //pvg
    procedure SetPopupMenu(Value: TPopupMenu);
  protected
    procedure DoChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 1;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crHand;
    property GrabCursor: TCursor read FGrabCursor write SetGrabCursor default crGrab; //pvg
    property PaperColor: TColor read FPaperColor write SetPaperColor default clWhite;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property ShadowWidth: TBorderWidth read FShadowWidth write SetShadowWidth default 3;
  end;

  { TPrintPreview}

  TThumbnailPreview = class;

  TPreviewPrintProgress = procedure(Sender: TObject; PageNo, Progress: Integer;
    var AbortIt: Boolean) of object;

  TPreviewAutoCustomForm = procedure(Sender: TObject; const CustomFormName: String;
    Operation: TOperation) of object;

  TPreviewPageDraw = procedure(Sender: TObject; PageNo: Integer; Canvas: TCanvas) of object;

  TVertAlign = (vaTop, vaCenter, vaBottom);  //rmk
  THorzAlign = (haLeft, haCenter, haRight);  //rmk

  TGrayscaleOption = (gsPreview, gsPrint);
  TGrayscaleOptions = set of TGrayscaleOption;

  TPreviewState = (psReady, psCreating, psPrinting, psEditing);

  TZoomState = (zsZoomOther, zsZoomToWidth, zsZoomToHeight, zsZoomToFit);

  TUnits = (mmPixel, mmLoMetric, mmHiMetric, mmLoEnglish, mmHiEnglish, mmTWIPS, mmPoints);

  TPaperType = (pLetter, pLetterSmall, pTabloid, pLedger, pLegal, pStatement,
    pExecutive, pA3, pA4, pA4Small, pA5, pB4, pB5, pFolio, pQuatro, p10x14,
    p11x17, pNote, pEnv9, pEnv10, pEnv11, pEnv12, pEnv14, pCSheet, pDSheet,
    pESheet, pEnvDL, pEnvC5, pEnvC3, pEnvC4, pEnvC6, pEnvC65, pEnvB4, pEnvB5,
    pEnvB6, pEnvItaly, pEnvMonarch, pEnvPersonal, pFanfoldUSStd, pFanfoldGermanStd,
    pFanfoldGermanLegal, pB4ISO, pJapanesePostcard, p9x11, p10x11, p15x11,
    pEnvInvite, pLetterExtra, pLegalExtra, TabloidExtra, pA4Extra, pLetterTransverse,
    pA4Transverse, pLetterExtraTransverse, pAPlus, pBPlus, pLetterPlus, pA4Plus,
    pA5Transverse, pB5Transverse, pA3Extra, pA5Extra, pB5Extra, pA2, pA3Transverse,
    pA3ExtraTransverse, pCustom);

  TPrintPreview = class(TScrollBox)
  private
    FThumbnailViews: TList;
    FPaperView: TPaperPreview;
    FPaperViewOptions: TPaperPreviewOptions;
    FPrintJobTitle: String;
    FPages: TMetafileList;
    FPageCanvas: TCanvas;
    FUnits: TUnits;
    FDeviceExt: TPoint;
    FPageExt: TPoint;
    FAborted: Boolean;
    FOrientation: TPrinterOrientation;
    FCurrentPage: Integer;
    FPaperType: TPaperType;
    FState: TPreviewState;
    FZoom: Integer;
    FZoomState: TZoomState;
    FZoomSavePos: Boolean;
    FZoomMin: Integer;
    FZoomMax: Integer;
    FZoomStep: Integer;
    FFastPrint: Boolean;                // obsolete
    FUsePrinterOptions: Boolean;
    FDirectPrint: Boolean;
    FDirectPrinting: Boolean;
    FDirectPrintPageCount: Integer;
    FOldMousePos: TPoint;
    FCanScrollHorz: Boolean;
    FCanScrollVert: Boolean;
    FIsDragging: Boolean;
    FEditingPage: Integer;
    FFormName: String;
    FAutoFormName: String;
    FAnnotation: Boolean;
    FBackground: Boolean;
    FGrayscale: TGrayscaleOptions;
    FOnBeginDoc: TNotifyEvent;
    FOnEndDoc: TNotifyEvent;
    FOnNewPage: TNotifyEvent;
    FOnEndPage: TNotifyEvent;
    FOnAbort: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnPrintProgress: TPreviewPrintProgress;
    FOnBeforePrint: TNotifyEvent;
    FOnAfterPrint: TNotifyEvent;
    FOnZoomChange: TNotifyEvent;
    FOnAutoCustomForm: TPreviewAutoCustomForm;
    FOnAnnotation: TPreviewPageDraw;
    FOnBackground: TPreviewPageDraw;
    PageMetafile: TMetafile;
    AnnotationMetafile: TMetafile;
    BackgroundMetafile: TMetafile;
    WheelAccumulator: Integer;
    procedure SetPaperViewOptions(Value: TPaperPreviewOptions);
    procedure SetUnits(Value: TUnits);
    procedure SetPaperType(Value: TPaperType);
    function GetPaperWidth: Integer;
    procedure SetPaperWidth(Value: Integer);
    function GetPaperHeight: Integer;
    procedure SetPaperHeight(Value: Integer);
    procedure SetPaperTypeByID(ID: Integer);
    procedure SetAnnotation(Value: Boolean);
    procedure SetBackground(Value: Boolean);
    procedure SetGrayscale(Value: TGrayscaleOptions);
    function GetFormName: String;
    procedure SetFormName(const Value: String);
    function GetPageBounds: TRect;
    function GetPrinterPageBounds: TRect;
    procedure SetOrientation(Value: TPrinterOrientation);
    procedure SetZoomState(Value: TZoomState);
    procedure SetZoom(Value: Integer);
    procedure SetZoomMin(Value: Integer);
    procedure SetZoomMax(Value: Integer);
    procedure SetCurrentPage(Value: Integer);
    function GetUseTempFile: Boolean;
    procedure SetUseTempFile(Value: Boolean);
    function GetTotalPages: Integer;
    function GetPages(PageNo: Integer): TMetafile;
    function GetCanvas: TCanvas;
    function GetPrinterInstalled: Boolean;
    function CalculateViewSize(const Space: TPoint): TPoint;
    function IsCustomPaper: Boolean;
    function IsZoomStored: Boolean;
    procedure PaperClick(Sender: TObject);
    procedure PaperDblClick(Sender: TObject);
    procedure PaperMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaperMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaperMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaperViewOptionsChanged(Sender: TObject);
    procedure PagesChanged(Sender: TObject);
    procedure PageChanged(Sender: TObject; PageIndex: Integer);
    procedure CNKeyDown(var Message: TWMKey); message CN_KEYDOWN;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure DoAnnotation(PageNo: Integer); virtual;
    procedure DoBackground(PageNo: Integer); virtual;
    procedure DoProgress(Current, Done, Total: Integer); virtual;
    procedure PaintPage(Sender: TObject; Canvas: TCanvas; const PageRect: TRect); virtual;
    function FindPaperType(APaperWidth, APaperHeight: Integer; InUnits: TUnits): TPaperType;
    procedure SetPaperSize(Width, Height: Integer); virtual;
    procedure CheckForAutoCustomForm;
    procedure CalculateMetafileSize;
    procedure CreateMetafileCanvas(out AMetafile: TMetafile; out ACanvas: TCanvas);
    procedure CloseMetafileCanvas(var AMetafile: TMetafile; var ACanvas: TCanvas);
    procedure CreatePrinterCanvas(out ACanvas: TCanvas);
    procedure ClosePrinterCanvas(var ACanvas: TCanvas);
    procedure ScaleCanvas(ACanvas: TCanvas);
    procedure RegisterThumbnailView(ThumbnailView: TThumbnailPreview);
    procedure UnregisterThumbnailView(ThumbnailView: TThumbnailPreview);
    procedure UpdateThumbnailViews(Rebuild: Boolean);
    procedure UpdateThumbnailPage(PageIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ConvertX(Value: Integer; InUnits, OutUnits: TUnits): Integer;
    function ConvertY(Value: Integer; InUnits, OutUnits: TUnits): Integer;
    function ConvertXY(X, Y: Integer; InUnits, OutUnits: TUnits): TPoint;
    procedure ConvertPoints(var Points; NumPoints: Integer; InUnits, OutUnits: TUnits);
    function ClientToPaper(const Pt: TPoint): TPoint;
    function PaperToClient(const Pt: TPoint): TPoint;
    function PaintGraphic(X, Y: Integer; Graphic: TGraphic): TPoint;
    function PaintGraphicEx(const Rect: TRect; Graphic: TGraphic;
      Proportinal, ShrinkOnly, Center: Boolean): TRect;
    function PaintGraphicEx2(const Rect: TRect; Graphic: TGraphic;   //rmk
      VertAlign: TVertAlign; HorzAlign: THorzAlign): TRect;      //rmk
    function PaintWinControl(X, Y: Integer; WinControl: TWinControl): TPoint;
    function PaintWinControlEx(const Rect: TRect; WinControl: TWinControl;
      Proportinal, ShrinkOnly, Center: Boolean): TRect;
    function PaintRichText(const Rect: TRect; RichEdit: TCustomRichEdit;
      MaxPages: Integer; pOffset: PInteger): Integer;
    function GetRichTextRect(var Rect: TRect; RichEdit: TCustomRichEdit;
      pOffset: PInteger): Integer;
    procedure Clear;
    function BeginEdit(PageNo: Integer): Boolean;
    procedure EndEdit;
    procedure BeginDoc;
    procedure EndDoc;
    procedure NewPage;
    procedure Abort;
    procedure Print;
    procedure UpdateZoom;
    procedure UpdateAnnotation;
    procedure UpdateBackground;
    procedure SetPrinterOptions;
    procedure GetPrinterOptions;
    function FetchFormNames(FormNames: TStrings): Boolean;
    function GetFormSize(const AFormName: String; out FormWidth, FormHeight: Integer): Boolean;
    function AddNewForm(const AFormName: String; FormWidth, FormHeight: DWORD): Boolean;
    function RemoveForm(const AFormName: String): Boolean;
    procedure PrintPages(FirstPage, LastPage: Integer);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure SaveAsPDF(const FileName: String);
    function CanSaveAsPDF: Boolean;
    property Aborted: Boolean read FAborted;
    property Canvas: TCanvas read GetCanvas;
    property TotalPages: Integer read GetTotalPages;
    property State: TPreviewState read FState;
    property PageSize: TPoint read FPageExt;
    property PagePixels: TPoint read FDeviceExt;
    property PageBounds: TRect read GetPageBounds;
    property PrinterPageBounds: TRect read GetPrinterPageBounds;
    property PrinterInstalled: Boolean read GetPrinterInstalled;
    property CurrentPage: Integer read FCurrentPage write SetCurrentPage;
    property FormName: String read GetFormName write SetFormName;
    property AutoFormName: String read FAutoFormName;
    property Pages[PageNo: Integer]: TMetafile read GetPages;
    property FastPrint: Boolean read FFastPrint write FFastPrint;       // obsolete
  published
    property Align default alClient;
    property Annotation: Boolean read FAnnotation write SetAnnotation default False;
    property Background: Boolean read FBackground write SetBackground default False;
    property DirectPrint: Boolean read FDirectPrint write FDirectPrint default False;
    property Grayscale: TGrayscaleOptions read FGrayscale write SetGrayscale default [];
    property Units: TUnits read FUnits write SetUnits default mmHiMetric;
    property Orientation: TPrinterOrientation read FOrientation write SetOrientation default poPortrait;
    property PaperType: TPaperType read FPaperType write SetPaperType default pA4;
    property PaperView: TPaperPreviewOptions read FPaperViewOptions write SetPaperViewOptions;
    property PaperWidth: Integer read GetPaperWidth write SetPaperWidth stored IsCustomPaper;
    property PaperHeight: Integer read GetPaperHeight write SetPaperHeight stored IsCustomPaper;
    property ParentFont default False;
    property PrintJobTitle: String read FPrintJobTitle write FPrintJobTitle;
    property TabStop default True;
    property UsePrinterOptions: Boolean read FUsePrinterOptions write FUsePrinterOptions default False;
    property UseTempFile: Boolean read GetUseTempFile write SetUseTempFile default False;
    property ZoomState: TZoomState read FZoomState write SetZoomState default zsZoomToFit;
    property Zoom: Integer read FZoom write SetZoom stored IsZoomStored;
    property ZoomMin: Integer read FZoomMin write SetZoomMin default 10;
    property ZoomMax: Integer read FZoomMax write SetZoomMax default 500;
    property ZoomSavePos: Boolean read FZoomSavePos write FZoomSavePos default True;
    property ZoomStep: Integer read FZoomStep write FZoomStep default 10;
    property OnBeginDoc: TNotifyEvent read FOnBeginDoc write FOnBeginDoc;
    property OnEndDoc: TNotifyEvent read FOnEndDoc write FOnEndDoc;
    property OnNewPage: TNotifyEvent read FOnNewPage write FOnNewPage;
    property OnEndPage: TNotifyEvent read FOnEndPage write FOnEndPage;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPrintProgress: TPreviewPrintProgress read FOnPrintProgress write FOnPrintProgress;
    property OnBeforePrint: TNotifyEvent read FOnBeforePrint write FOnBeforePrint;
    property OnAfterPrint: TNotifyEvent read FOnAfterPrint write FOnAfterPrint;
    property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
    property OnAutoCustomForm: TPreviewAutoCustomForm read FOnAutoCustomForm write FOnAutoCustomForm;
    property OnAnnotation: TPreviewPageDraw read FOnAnnotation write FOnAnnotation;
    property OnBackground: TPreviewPageDraw read FOnBackground write FOnBackground;
  end;

  { TThumbnailPreview }

  TThumbnailClass = class of TThumbnail;

  TThumbnail = class(TObject)
  protected
    PageNo: Integer;
    PageView: TPaperPreview;
    PageLabel: TLabel;
  public
    constructor Create(AOwner: TThumbnailPreview; APageNo: Integer); virtual;
    destructor Destroy; override;
    function GetBoundRect: TRect; virtual;
    function HasAsMember(Component: TComponent): Boolean; virtual;
  end;

  TThumbnailPreview = class(TScrollBox)
  private
    FThumbnails: TList;
    FZoom: Integer;
    FMargin: Byte;
    FMarkerColor: TColor;
    FOrientation: TScrollBarKind;
    FPrintPreview: TPrintPreview;
    FPaperViewOptions: TPaperPreviewOptions;
    FRowCount: Integer;
    FColCount: Integer;
    FThumbnailClass: TThumbnailClass;
    FOnChange: TNotifyEvent;
    WheelAccumulator: Integer;
    ActiveThumb: TThumbnail;
    Updating: Boolean;
    procedure SetZoom(Value: Integer);
    procedure SetMargin(Value: Byte);
    procedure SetMarkerColor(Value: TColor);
    procedure SetOrientation(Value: TScrollBarKind);
    procedure SetPrintPreview(Value: TPrintPreview);
    procedure SetThumbnailClass(Value: TThumbnailClass);
    procedure SetPaperViewOptions(Value: TPaperPreviewOptions);
    procedure PaperViewOptionsChanged(Sender: TObject);
    procedure ThumbnailClick(Sender: TObject);
    procedure ThumbnailPaint(Sender: TObject; Canvas: TCanvas; const PageRect: TRect);
    procedure CNKeyDown(var Message: TWMKey); message CN_KEYDOWN;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMMouseWheel(var Message: TMessage); message WM_MOUSEWHEEL;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
    procedure MakeVisible(const Bounds: TRect); virtual;
    procedure UpdateThumbnails(Rebuild: Boolean); virtual;
    procedure UpdatePage(Index: Integer); virtual;
    procedure CalculateElementBounds(out ViewPos, ViewSize, LabelPos,
      LabelSize, ThumbSize: TPoint); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property ThumbnailClass: TThumbnailClass read FThumbnailClass write SetThumbnailClass;
  published
    property Align default alLeft;
    property Margin: Byte read FMargin write SetMargin default 6;
    property MarkerColor: TColor read FMarkerColor write SetMarkerColor default clBlue;
    property Orientation: TScrollBarKind read FOrientation write SetOrientation default sbVertical;
    property PrintPreview: TPrintPreview read FPrintPreview write SetPrintPreview;
    property PaperView: TPaperPreviewOptions read FPaperViewOptions write SetPaperViewOptions;
    property TabStop default True;
    property Zoom: Integer read FZoom write SetZoom default 10;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPaperSizeInfo = record
    ID: SmallInt;
    Width, Height: Integer;
    Units: TUnits;
  end;

const
  // Paper Sizes
  PaperSizes: array[TPaperType] of TPaperSizeInfo = (
    (ID: DMPAPER_LETTER;                  Width: 08500;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_LETTER;                  Width: 08500;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_TABLOID;                 Width: 11000;     Height: 17000;     Units: mmHiEnglish),
    (ID: DMPAPER_LEDGER;                  Width: 17000;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_LEGAL;                   Width: 08500;     Height: 14000;     Units: mmHiEnglish),
    (ID: DMPAPER_STATEMENT;               Width: 05500;     Height: 08500;     Units: mmHiEnglish),
    (ID: DMPAPER_EXECUTIVE;               Width: 07250;     Height: 10500;     Units: mmHiEnglish),
    (ID: DMPAPER_A3;                      Width: 02970;     Height: 04200;     Units: mmLoMetric),
    (ID: DMPAPER_A4;                      Width: 02100;     Height: 02970;     Units: mmLoMetric),
    (ID: DMPAPER_A4SMALL;                 Width: 02100;     Height: 02970;     Units: mmLoMetric),
    (ID: DMPAPER_A5;                      Width: 01480;     Height: 02100;     Units: mmLoMetric),
    (ID: DMPAPER_B4;                      Width: 02500;     Height: 03540;     Units: mmLoMetric),
    (ID: DMPAPER_B5;                      Width: 01820;     Height: 02570;     Units: mmLoMetric),
    (ID: DMPAPER_FOLIO;                   Width: 08500;     Height: 13000;     Units: mmHiEnglish),
    (ID: DMPAPER_QUARTO;                  Width: 02150;     Height: 02750;     Units: mmLoMetric),
    (ID: DMPAPER_10X14;                   Width: 10000;     Height: 14000;     Units: mmHiEnglish),
    (ID: DMPAPER_11X17;                   Width: 11000;     Height: 17000;     Units: mmHiEnglish),
    (ID: DMPAPER_NOTE;                    Width: 08500;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_9;                   Width: 03875;     Height: 08875;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_10;                  Width: 04125;     Height: 09500;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_11;                  Width: 04500;     Height: 10375;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_12;                  Width: 04750;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_14;                  Width: 05000;     Height: 11500;     Units: mmHiEnglish),
    (ID: DMPAPER_CSHEET;                  Width: 17000;     Height: 22000;     Units: mmHiEnglish),
    (ID: DMPAPER_DSHEET;                  Width: 22000;     Height: 34000;     Units: mmHiEnglish),
    (ID: DMPAPER_ESHEET;                  Width: 34000;     Height: 44000;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_DL;                  Width: 01100;     Height: 02200;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_C5;                  Width: 01620;     Height: 02290;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_C3;                  Width: 03240;     Height: 04580;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_C4;                  Width: 02290;     Height: 03240;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_C6;                  Width: 01140;     Height: 01620;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_C65;                 Width: 01140;     Height: 02290;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_B4;                  Width: 02500;     Height: 03530;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_B5;                  Width: 01760;     Height: 02500;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_B6;                  Width: 01760;     Height: 01250;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_ITALY;               Width: 01100;     Height: 02300;     Units: mmLoMetric),
    (ID: DMPAPER_ENV_MONARCH;             Width: 03875;     Height: 07500;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_PERSONAL;            Width: 03625;     Height: 06500;     Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_US;              Width: 14875;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_STD_GERMAN;      Width: 08500;     Height: 12000;     Units: mmHiEnglish),
    (ID: DMPAPER_FANFOLD_LGL_GERMAN;      Width: 08500;     Height: 13000;     Units: mmHiEnglish),
    (ID: DMPAPER_ISO_B4;                  Width: 02500;     Height: 03530;     Units: mmLoMetric),
    (ID: DMPAPER_JAPANESE_POSTCARD;       Width: 01000;     Height: 01480;     Units: mmLoMetric),
    (ID: DMPAPER_9X11;                    Width: 09000;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_10X11;                   Width: 10000;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_15X11;                   Width: 15000;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_ENV_INVITE;              Width: 02200;     Height: 02200;     Units: mmLoMetric),
    (ID: DMPAPER_LETTER_EXTRA;            Width: 09500;     Height: 12000;     Units: mmHiEnglish),
    (ID: DMPAPER_LEGAL_EXTRA;             Width: 09500;     Height: 15000;     Units: mmHiEnglish),
    (ID: DMPAPER_TABLOID_EXTRA;           Width: 11690;     Height: 18000;     Units: mmHiEnglish),
    (ID: DMPAPER_A4_EXTRA;                Width: 09270;     Height: 12690;     Units: mmHiEnglish),
    (ID: DMPAPER_LETTER_TRANSVERSE;       Width: 08500;     Height: 11000;     Units: mmHiEnglish),
    (ID: DMPAPER_A4_TRANSVERSE;           Width: 02100;     Height: 02970;     Units: mmLoMetric),
    (ID: DMPAPER_LETTER_EXTRA_TRANSVERSE; Width: 09500;     Height: 12000;     Units: mmHiEnglish),
    (ID: DMPAPER_A_PLUS;                  Width: 02270;     Height: 03560;     Units: mmLoMetric),
    (ID: DMPAPER_B_PLUS;                  Width: 03050;     Height: 04870;     Units: mmLoMetric),
    (ID: DMPAPER_LETTER_PLUS;             Width: 08500;     Height: 12690;     Units: mmHiEnglish),
    (ID: DMPAPER_A4_PLUS;                 Width: 02100;     Height: 03300;     Units: mmLoMetric),
    (ID: DMPAPER_A5_TRANSVERSE;           Width: 01480;     Height: 02100;     Units: mmLoMetric),
    (ID: DMPAPER_B5_TRANSVERSE;           Width: 01820;     Height: 02570;     Units: mmLoMetric),
    (ID: DMPAPER_A3_EXTRA;                Width: 03220;     Height: 04450;     Units: mmLoMetric),
    (ID: DMPAPER_A5_EXTRA;                Width: 01740;     Height: 02350;     Units: mmLoMetric),
    (ID: DMPAPER_B5_EXTRA;                Width: 02010;     Height: 02760;     Units: mmLoMetric),
    (ID: DMPAPER_A2;                      Width: 04200;     Height: 05940;     Units: mmLoMetric),
    (ID: DMPAPER_A3_TRANSVERSE;           Width: 02970;     Height: 04200;     Units: mmLoMetric),
    (ID: DMPAPER_A3_EXTRA_TRANSVERSE;     Width: 03220;     Height: 04450;     Units: mmLoMetric),
    (ID: DMPAPER_USER;                    Width: 0;         Height: 0;         Units: mmPixel));

function ConvertUnits(Value, DPI: Integer; InUnits, OutUnits: TUnits): Integer;

procedure DrawGraphic(Canvas: TCanvas; X, Y: Integer; Graphic: TGraphic);
procedure StretchDrawGraphic(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);

procedure DrawGrayscale(Canvas: TCanvas; X, Y: Integer; Graphic: TGraphic);
procedure StretchDrawGrayscale(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);

procedure ConvertBitmapToGrayscale(Bitmap: TBitmap);

procedure Register;

implementation

uses
  RichEdit {$IFDEF ZLIB}, ZLib, Types {$ENDIF};

const
  dsPDF_lib = 'dspdf.dll';

type
  TdsPDF = record
    Handle: HMODULE;
    BeginDoc: function(FileName: PChar): Integer; stdcall;
    EndDoc: function: Integer; stdcall;
    NewPage: function: Integer; stdcall;
    PrintPageMemory: function(Data: Pointer; Size: Integer): Integer; stdcall;
    PrintPageFile: function(FileName: PChar): Integer; stdcall;
    SetParameters: function(AOffsetX, AOffsetY: Integer; AConverterX, AConverterY: Double): Integer; stdcall;
    SetPage: function(ps, orientation, w, h: Integer): Integer; stdcall;
  end;

var
  dsPDF: TdsPDF;

{$R *.RES}

procedure Register;
begin
  RegisterComponents('Delphi Area', [TPaperPreview, TThumbnailPreview, TPrintPreview]);
end;

{ Helper Functions }

const
  ZLibSignature        = $9C78;
  PageInfoSignature    = $13490208;
  MetafilesSignature   = $50502D4B;
  SNotEnoughMemory     = 'Not enough memory to create a new page';
  SInvalidPreviewData  = 'The content is not Print Preview data';

function GetTemporaryFileName: String;
var
  TempPath: array[0..255] of Char;
  TempFile: array[0..255] of Char;
begin
  GetTempPath(SizeOf(TempPath), TempPath);
  GetTempFileName(TempPath, 'PP', 0, TempFile);
  Result := StrPas(TempFile);
end;

{$IFDEF IMAGE_TRANSPARENCY}
procedure TransparentStretchDIBits(dstDC: HDC;
  dstX, dstY: Integer; dstW, dstH: Integer;
  srcX, srcY: Integer; srcW, srcH: Integer;
  bmpBits: Pointer; var bmpInfo: TBitmapInfo;
  mskBits: Pointer; var mskInfo: TBitmapInfo;
  Usage: DWORD);
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  memInfo: pBitmapInfo;
  memBits: Pointer;
  HeaderSize: DWORD;
  ImageSize: DWORD;
begin
  MemDC := CreateCompatibleDC(0);
  try
    MemBmp := CreateCompatibleBitmap(dstDC, srcW, srcH);
    try
      Save := SelectObject(MemDC, MemBmp);
      SetStretchBltMode(MemDC, ColorOnColor);
      StretchDIBits(MemDC, 0, 0, srcW, srcH, 0, 0, srcW, srcH, mskBits, mskInfo, Usage, SrcCopy);
      StretchDIBits(MemDC, 0, 0, srcW, srcH, 0, 0, srcW, srcH, bmpBits, bmpInfo, Usage, SrcErase);
      if Save <> 0 then SelectObject(MemDC, Save);
      GetDIBSizes(MemBmp, HeaderSize, ImageSize);
      GetMem(memInfo, HeaderSize);
      try
        GetMem(memBits, ImageSize);
        try
          GetDIB(MemBmp, 0, memInfo^, memBits^);
          crText := SetTextColor(dstDC, RGB(0, 0, 0));
          crBack := SetBkColor(dstDC, RGB(255, 255, 255));
          SetStretchBltMode(dstDC, ColorOnColor);
          StretchDIBits(dstDC, dstX, dstY, dstW, dstH, srcX, srcY, srcW, srcH, mskBits, mskInfo, Usage, SrcAnd);
          StretchDIBits(dstDC, dstX, dstY, dstW, dstH, srcX, srcY, srcW, srcH, memBits, memInfo^, Usage, SrcInvert);
          SetTextColor(dstDC, crText);
          SetBkColor(dstDC, crBack);
        finally
          FreeMem(memBits);
        end;
      finally
        FreeMem(memInfo);
      end;
    finally
      DeleteObject(MemBmp);
    end;
  finally
    DeleteDC(MemDC);
  end;
end;
{$ENDIF}

procedure DrawBitmapAsDIB(DC: HDC; Bitmap: TBitmap; const Rect: TRect);
var
  BitmapHeader: pBitmapInfo;
  BitmapImage: Pointer;
  HeaderSize: DWORD;
  ImageSize: DWORD;
  {$IFDEF IMAGE_TRANSPARENCY}
  MaskBitmapHeader: pBitmapInfo;
  MaskBitmapImage: Pointer;
  maskHeaderSize: DWORD;
  MaskImageSize: DWORD;
  {$ENDIF}
begin
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  try
    GetMem(BitmapImage, ImageSize);
    try
      GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
      {$IFDEF IMAGE_TRANSPARENCY}
      if Bitmap.Transparent then
      begin
        GetDIBSizes(Bitmap.MaskHandle, MaskHeaderSize, MaskImageSize);
        GetMem(MaskBitmapHeader, MaskHeaderSize);
        try
          GetMem(MaskBitmapImage, MaskImageSize);
          try
            GetDIB(Bitmap.MaskHandle, 0, MaskBitmapHeader^, MaskBitmapImage^);
            TransparentStretchDIBits(
              DC,                              // handle of destination device context
              Rect.Left, Rect.Top,             // upper-left corner of destination rectagle
              Rect.Right - Rect.Left,          // width of destination rectagle
              Rect.Bottom - Rect.Top,          // height of destination rectagle
              0, 0,                            // upper-left corner of source rectangle
              Bitmap.Width, Bitmap.Height,     // width and height of source rectangle
              BitmapImage,                     // address of bitmap bits
              BitmapHeader^,                   // bitmap data
              MaskBitmapImage,                 // address of mask bitmap bits
              MaskBitmapHeader^,               // mask bitmap data
              DIB_RGB_COLORS                   // usage: the color table contains literal RGB values
            );
          finally
            FreeMem(MaskBitmapImage)
          end;
        finally
          FreeMem(MaskBitmapHeader);
        end;
      end
      else
      {$ENDIF}
      begin
        SetStretchBltMode(DC, ColorOnColor);
        StretchDIBits(
          DC,                                  // handle of destination device context
          Rect.Left, Rect.Top,                 // upper-left corner of destination rectagle
          Rect.Right - Rect.Left,              // width of destination rectagle
          Rect.Bottom - Rect.Top,              // height of destination rectagle
          0, 0,                                // upper-left corner of source rectangle
          Bitmap.Width, Bitmap.Height,         // width and height of source rectangle
          BitmapImage,                         // address of bitmap bits
          BitmapHeader^,                       // bitmap data
          DIB_RGB_COLORS,                      // usage: the color table contains literal RGB values
          SrcCopy                              // raster operation code: copy source pixels
        );
      end;
    finally
      FreeMem(BitmapImage)
    end;
  finally
    FreeMem(BitmapHeader);
  end;
end;

procedure StretchDrawGraphic(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);
var
  Bitmap: TBitmap;
begin
  if Graphic is TBitmap then
    DrawBitmapAsDIB(Canvas.Handle, TBitmap(Graphic), Rect)
  else if Graphic is TMetafile then
    Canvas.StretchDraw(Rect, Graphic)
  else if Graphic is TIcon then
    DrawIconEx(Canvas.Handle, Rect.Left, Rect.Top, TIcon(Graphic).Handle,
      Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, 0, 0, DI_NORMAL)
  else
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Canvas.Brush.Color := clFuchsia;
      Bitmap.Width := Graphic.Width;
      Bitmap.Height := Graphic.Height;
      Bitmap.Canvas.Draw(0, 0, Graphic);
      Bitmap.Transparent := Graphic.Transparent;
      DrawBitmapAsDIB(Canvas.Handle, Bitmap, Rect)
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure DrawGraphic(Canvas: TCanvas; X, Y: Integer; Graphic: TGraphic);
var
  Rect: TRect;
begin
  Rect.Left := X;
  Rect.Top := Y;
  Rect.Right := X + Graphic.Width;
  Rect.Bottom := Y + Graphic.Height;
  StretchDrawGraphic(Canvas, Rect, Graphic);
end;

procedure StretchDrawGrayscale(Canvas: TCanvas; const Rect: TRect; Graphic: TGraphic);
var
  Bitmap: TBitmap;
  R: TRect;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Rect.Right - Rect.Left;
    Bitmap.Height := Rect.Bottom - Rect.Top;
    SetRect(R, 0, 0, Bitmap.Width, Bitmap.Height);
    Bitmap.Canvas.StretchDraw(R, Graphic);
    ConvertBitmapToGrayscale(Bitmap);
    DrawBitmapAsDIB(Canvas.Handle, Bitmap, Rect);
  finally
    Bitmap.Free;
  end;
end;

procedure DrawGrayscale(Canvas: TCanvas; X, Y: Integer; Graphic: TGraphic);
var
  Rect: TRect;
begin
  Rect.Left := X;
  Rect.Top := Y;
  Rect.Right := X + Graphic.Width;
  Rect.Bottom := Y + Graphic.Height;
  StretchDrawGrayscale(Canvas, Rect, Graphic);
end;

procedure ConvertBitmapToGrayscale(Bitmap: TBitmap);
var
  LogPalette: PLogPalette;
  NumEntries: Word;
  Intensity: Byte;
  I: Integer;
  GrayPalette: HPALETTE;
begin
  Bitmap.PixelFormat := pf8bit;
  GetObject(Bitmap.Palette, SizeOf(NumEntries), @NumEntries);
  GetMem(LogPalette, SizeOf(TLogPalette) + NumEntries * SizeOf(TPaletteEntry));
  try
    with LogPalette^ do
    begin
      palVersion := $300;
      palNumEntries := NumEntries;
      GetPaletteEntries(Bitmap.Palette, 0, NumEntries, palPalEntry[0]);
    end;
    for I := 0 to NumEntries - 1 do
      with LogPalette^.palPalEntry[I] do
      begin
        Intensity := (peRed * 30 + peGreen * 59 + peBlue * 11) div 100;
        peRed := Intensity;
        peGreen := Intensity;
        peBlue := Intensity;
        peFlags := 0;
      end;
      GrayPalette := CreatePalette(LogPalette^);
      try
        Bitmap.Palette := GrayPalette;
      finally
        DeleteObject(GrayPalette);
      end;
  finally
    FreeMem(LogPalette);
  end;
end;

{ TMetafileList }

constructor TMetafileList.Create;
begin
  inherited Create;
  FRecords := TList.Create;
  FLoadedMetafile := TMetafile.Create;
  FLoadedIndex := -1;
end;

destructor TMetafileList.Destroy;
begin
  Cleanup;
  FLoadedMetafile.Free;
  FRecords.Free;
  inherited Destroy;
end;

procedure TMetafileList.Cleanup;
begin
  FLoadedMetafile.OnChange := nil;
  FLoadedMetafile.Clear;
  FRecords.Clear;
  FLoadedIndex := -1;
  if Assigned(FDataStream) then
  begin
    FDataStream.Free;
    FDataStream := nil;
    if FUseTempFile and FileExists(FTempFile) then
      DeleteFile(FTempFile);
  end;
end;

procedure TMetafileList.Clear;
begin
  Cleanup;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TMetafileList.Add(AMetafile: TMetafile): Integer;
var
  Offset: Integer;
begin
  if not Assigned(FDataStream) then
    FDataStream := CreateMetafileStream;
  FDataStream.Seek(0, soFromEnd);
  Offset := FDataStream.Position;
  AMetafile.SaveToStream(FDataStream);
  Result := FRecords.Add(Pointer(Offset));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMetafileList.LoadFromStream(Stream: TStream);
var
  I: Integer;
  Data: Integer;
  ReadBytes: Integer;
  Buffer: array[1..$F000] of Byte;
begin
  Stream.Read(Data, SizeOf(Data));
  if MetafilesSignature <> Data then
    raise EInvalidPreviewData.Create(SInvalidPreviewData);
  Clear;
  Stream.Read(Data, SizeOf(Data));
  FRecords.Capacity := Data;
  for I := Data downto 1 do
  begin
    Stream.Read(Data, SizeOf(Data));
    FRecords.Add(Pointer(Data));
  end;
  FDataStream := CreateMetafileStream;
  ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
  while ReadBytes > 0 do
  begin
    FDataStream.Write(Buffer, ReadBytes);
    ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
  end;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMetafileList.SaveToStream(Stream: TStream);
var
  I: Integer;
  Data: Integer;
  ReadBytes: Integer;
  Buffer: array[1..$F000] of Byte;
begin
  Data := MetafilesSignature;
  Stream.Write(Data, SizeOf(Data));
  Data := FRecords.Count;
  Stream.Write(Data, SizeOf(Data));
  for I := 0 to FRecords.Count - 1 do
  begin
    Data := Integer(FRecords[I]);
    Stream.Write(Data, SizeOf(Data));
  end;
  if Assigned(FDataStream) then
  begin
    FDataStream.Position := 0;
    ReadBytes := FDataStream.Read(Buffer, SizeOf(Buffer));
    while ReadBytes > 0 do
    begin
      Stream.Write(Buffer, ReadBytes);
      ReadBytes := FDataStream.Read(Buffer, SizeOf(Buffer));
    end;
  end;
end;

procedure TMetafileList.LoadFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TMetafileList.SaveToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

function TMetafileList.GetCount: Integer;
begin
  Result := FRecords.Count;
end;

function TMetafileList.GetItems(Index: Integer): TMetafile;
begin
  LoadedIndex := Index;
  Result := FLoadedMetafile;
end;

procedure TMetafileList.SetLoadedIndex(Value: Integer);
begin
  if FLoadedIndex <> Value then
  begin
    FLoadedMetafile.OnChange := nil;
    try
      FDataStream.Seek(Integer(FRecords[Value]), soFromBeginning);
      FLoadedMetafile.LoadFromStream(FDataStream);
      FLoadedIndex := Value;
    finally
      FLoadedMetafile.OnChange := MetafileChanged;
    end;
  end;
end;

procedure TMetafileList.SetUseTempFile(Value: Boolean);
var
  NewStream: TStream;
begin
  if FUseTempFile <> Value then
  begin
    FUseTempFile := Value;
    if Assigned(FDataStream) then
    begin
      NewStream := CreateMetafileStream;
      NewStream.CopyFrom(FDataStream, 0);
      FDataStream.Free;
      FDataStream := NewStream;
      if not FUseTempFile and FileExists(FTempFile) then
      begin
        DeleteFile(FTempFile);
        FTempFile := EmptyStr;
      end;
    end;
  end;
end;

function TMetafileList.CreateMetafileStream: TStream;
begin
  if FUseTempFile then
  begin
    FTempFile := GetTemporaryFileName;
    Result := TFileStream.Create(FTempFile, fmCreate or fmShareExclusive)
  end
  else
    Result := TMemoryStream.Create;
end;

procedure TMetafileList.MetafileChanged(Sender: TObject);
var
  Stream, RestOfStream: TMemoryStream;
  OldSize, Delta, I: Integer;
begin
  RestOfStream := nil;
  Stream := TMemoryStream.Create;
  try
    FLoadedMetafile.SaveToStream(Stream);
    if FLoadedIndex < FRecords.Count - 1 then
      OldSize := Integer(FRecords[FLoadedIndex+1]) - Integer(FRecords[FLoadedIndex])
    else
      OldSize := FDataStream.Size - Integer(FRecords[FLoadedIndex]);
    Delta := Stream.Size - OldSize;
    if (Delta <> 0) and (FLoadedIndex < FRecords.Count - 1) then
    begin
      RestOfStream := TMemoryStream.Create;
      FDataStream.Seek(Integer(FRecords[FLoadedIndex+1]), soFromBeginning);
      RestOfStream.Size := FDataStream.Size - FDataStream.Position;
      RestOfStream.CopyFrom(FDataStream, RestOfStream.Size);
    end;
    FDataStream.Seek(Integer(FRecords[FLoadedIndex]), soFromBeginning);
    FDataStream.CopyFrom(Stream, 0);
  finally
    Stream.Free;
  end;
  if RestOfStream <> nil then
  begin
    for I := FLoadedIndex + 1 to FRecords.Count - 1 do
      FRecords[I] := Pointer(Integer(FRecords[I]) + Delta);
    FDataStream.CopyFrom(RestOfStream, 0);
    RestOfStream.Free;
  end;
  if Delta < 0 then
    FDataStream.Size := FDataStream.Size + Delta;
  if Assigned(FOnCurrentChange) then
    FOnCurrentChange(Self, FLoadedIndex);
end;

{ TPaperPreview }

constructor TPaperPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FOffScreen := TBitmap.Create;
  FBorderColor := clBlack;
  FBorderSize := 1;
  FPaperColor := clWhite;
  FShadowColor := clBtnShadow;
  FShadowSize := 3;
  PaperWidth := 105;
  PaperHeight := 148;
end;

destructor TPaperPreview.Destroy;
begin
  FOffScreen.Free;
  inherited Destroy;
end;

procedure TPaperPreview.Invalidate;
begin
  FOffScreenDrawn := False;
  inherited Invalidate;
end;

procedure TPaperPreview.Paint;
var
  Rect: TRect;
  ClipRgn: THandle;
begin
  if FOffScreenValid then
  begin
    if not FOffScreenPrepared then
    begin
      PrepareOffScreen;
      FOffScreenPrepared := True;
      FOffScreenDrawn := False;
    end;
    if not FOffScreenDrawn then
    begin
      GetPageRect(Rect);
      ClipRgn := CreateRectRgnIndirect(Rect);
      try
        SelectClipRgn(FOffscreen.Canvas.Handle, ClipRgn);
      finally
        DeleteObject(ClipRgn);
      end;
      FOffscreen.Canvas.Brush.Style := bsSolid;
      FOffscreen.Canvas.Brush.Color := PaperColor;
      FOffscreen.Canvas.FillRect(Rect);
      if Assigned(FOnPaint) then
        FOnPaint(Self, FOffScreen.Canvas, Rect);
      SelectClipRgn(FOffscreen.Canvas.Handle, 0);
      FOffScreenDrawn := True;
    end;
    BitBlt(Canvas.Handle, 0, 0, Width, Height, FOffScreen.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TPaperPreview.PrepareOffScreen;
var
  Rect: TRect;
begin
  with FOffscreen.Canvas do
  begin
    Pen.Mode := pmCopy;
    if BorderSize > 0 then
    begin
      Pen.Width := BorderSize;
      Pen.Style := psInsideFrame;
      Pen.Color := BorderColor;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width - ShadowSize, Height - ShadowSize);
    end;
    if ShadowSize > 0 then
    begin
      Brush.Style := bsSolid;
      Brush.Color := ShadowColor;
      SetRect(Rect, Width - ShadowSize, ShadowSize, Width, Height);
      FillRect(Rect);
      SetRect(Rect, ShadowSize, Height - ShadowSize, Width, Height);
      FillRect(Rect);
      Brush.Color := Color;
      SetRect(Rect, Width - ShadowSize, 0, Width, ShadowSize);
      FillRect(Rect);
      SetRect(Rect, 0, Height - ShadowSize, ShadowSize, Height);
      FillRect(Rect);
    end;
  end;
end;

procedure TPaperPreview.GetPageRect(var Rect: TRect);
begin
  with Rect do
  begin
    Left := BorderSize;
    Top := BorderSize;
    Right := Width - (BorderSize + ShadowSize);
    Bottom := Height - (BorderSize + ShadowSize);
  end;
end;

function TPaperPreview.ActualSize(Value: Integer): Integer;
begin
  Result := Value + 2 * FBorderSize + FShadowSize;
end;

function TPaperPreview.LogicalSize(Value: Integer): Integer;
begin
  Result := Value - 2 * FBorderSize - FShadowSize;
end;

procedure TPaperPreview.SetPaperWidth(Value: Integer);
begin
  Width := ActualSize(Value);
end;

function TPaperPreview.GetPaperWidth: Integer;
begin
  Result := LogicalSize(Width);
end;

procedure TPaperPreview.SetPaperHeight(Value: Integer);
begin
  Height := ActualSize(Value);
end;

function TPaperPreview.GetPaperHeight: Integer;
begin
  Result := LogicalSize(Height);
end;

procedure TPaperPreview.SetPaperColor(Value: TColor);
begin
  if FPaperColor <> Value then
  begin
    FPaperColor := Value;
    FOffScreenPrepared := False;
    Invalidate;
  end;
end;

procedure TPaperPreview.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    FOffScreenPrepared := False;
    Invalidate;
  end;
end;

procedure TPaperPreview.SetBorderSize(Value: TBorderWidth);
var
  PaperSize: TPoint;
begin
  if FBorderSize <> Value then
  begin
    PaperSize.X := PaperWidth;
    PaperSize.Y := PaperHeight;
    FBorderSize := Value;
    PaperWidth := PaperSize.X;
    PaperHeight := PaperSize.Y;
  end;
end;

procedure TPaperPreview.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    FOffScreenPrepared := False;
    Invalidate;
  end;
end;

procedure TPaperPreview.SetShadowSize(Value: TBorderWidth);
var
  PaperSize: TPoint;
begin
  if FShadowSize <> Value then
  begin
    PaperSize.X := PaperWidth;
    PaperSize.Y := PaperHeight;
    FShadowSize := Value;
    PaperWidth := PaperSize.X;
    PaperHeight := PaperSize.Y;
  end;
end;

procedure TPaperPreview.WMSize(var Message: TWMSize);
begin
  inherited;
  try
    FOffScreen.Width := Width;
    FOffScreen.Height := Height;
    FOffScreenValid := True;
    FOffScreenPrepared := False;
  except
    FOffScreenValid := False;
  end;
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TPaperPreview.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TPaperPreviewOptions }

constructor TPaperPreviewOptions.Create;
begin
  inherited Create;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FCursor := crDefault;
  FDragCursor := crHand;
  FGrabCursor := crGrab;  //pvg
  FPaperColor := clWhite;
  FShadowColor := clBtnShadow;
  FShadowWidth := 3;
end;

procedure TPaperPreviewOptions.Assign(Source: TPersistent);
begin
  if Source is TPaperPreviewOptions then
  begin
    BorderColor := TPaperPreviewOptions(Source).BorderColor;
    BorderWidth :=  TPaperPreviewOptions(Source).BorderWidth;
    Cursor := TPaperPreviewOptions(Source).Cursor;
    DragCursor := TPaperPreviewOptions(Source).DragCursor;
    GrabCursor := TPaperPreviewOptions(Source).GrabCursor; //pvg
    PaperColor := TPaperPreviewOptions(Source).PaperColor;
    PopupMenu := TPaperPreviewOptions(Source).PopupMenu;
    ShadowColor := TPaperPreviewOptions(Source).ShadowColor;
    ShadowWidth := TPaperPreviewOptions(Source).ShadowWidth;
  end
  else
    inherited Assign(Source);
end;

procedure TPaperPreviewOptions.AssignTo(Source: TPersistent);
begin
  if Source is TPaperPreviewOptions then
    Source.Assign(Self)
  else if Source is TPaperPreview then
  begin
    TPaperPreview(Source).PaperColor := PaperColor;
    TPaperPreview(Source).BorderColor := BorderColor;
    TPaperPreview(Source).BorderSize := BorderWidth;
    TPaperPreview(Source).ShadowColor := ShadowColor;
    TPaperPreview(Source).ShadowSize := ShadowWidth;
    TPaperPreview(Source).Cursor := Cursor;
    TPaperPreview(Source).PopupMenu := PopupMenu;
  end
  else
    inherited AssignTo(Source);
end;

procedure TPaperPreviewOptions.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(self);
end;

procedure TPaperPreviewOptions.SetPaperColor(Value: TColor);
begin
  if FPaperColor <> Value then
  begin
    FPaperColor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetBorderWidth(Value: TBorderWidth);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetShadowColor(Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetShadowWidth(Value: TBorderWidth);
begin
  if FShadowWidth <> Value then
  begin
    FShadowWidth := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetCursor(Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetDragCursor(Value: TCursor);
begin
  if FDragCursor <> Value then
  begin
    FDragCursor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetGrabCursor(Value: TCursor); //pvg
begin
  if FGrabCursor <> Value then
  begin
    FGrabCursor := Value;
    DoChange;
  end;
end;

procedure TPaperPreviewOptions.SetPopupMenu(Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    DoChange;
  end;
end;

{ TPrintPreview }

procedure RaiseOutOfMemory;
begin
  raise EOutOfMemory.Create(SNotEnoughMemory);
end;

procedure SwapValues(var A, B: Integer);
begin
  A := A xor B;
  B := A xor B;
  A := A xor B;
end;

function ConvertUnits(Value, DPI: Integer; InUnits, OutUnits: TUnits): Integer;
begin
  Result := Value;
  case InUnits of
    mmLoMetric:
      case OutUnits of
        mmLoMetric: Result := Value;
        mmHiMetric: Result := Value * 10;
        mmLoEnglish: Result := MulDiv(Value, 100, 254);
        mmHiEnglish: Result := MulDiv(Value, 1000, 254);
        mmPoints: Result := MulDiv(Value, 72, 254);
        mmTWIPS: Result := MulDiv(Value, 1440, 254);
        mmPixel: Result := MulDiv(Value, DPI, 254);
      end;
    mmHiMetric:
      case OutUnits of
        mmLoMetric: Result := Value div 10;
        mmHiMetric: Result := Value;
        mmLoEnglish: Result := MulDiv(Value, 10, 254);
        mmHiEnglish: Result := MulDiv(Value, 100, 254);
        mmPoints: Result := MulDiv(Value, 72, 2540);
        mmTWIPS: Result := MulDiv(Value, 1440, 2540);
        mmPixel: Result := MulDiv(Value, DPI, 2540);
      end;
    mmLoEnglish:
      case OutUnits of
        mmLoMetric: Result := MulDiv(Value, 254, 100);
        mmHiMetric: Result := MulDiv(Value, 254, 10);
        mmLoEnglish: Result := Value;
        mmHiEnglish: Result := Value * 10;
        mmPoints: Result := MulDiv(Value, 72, 100);
        mmTWIPS: Result := MulDiv(Value, 1440, 100);
        mmPixel: Result := MulDiv(Value, DPI, 100);
      end;
    mmHiEnglish:
      case OutUnits of
        mmLoMetric: Result := MulDiv(Value, 254, 1000);
        mmHiMetric: Result := MulDiv(Value, 254, 100);
        mmLoEnglish: Result := Value div 10;
        mmHiEnglish: Result := Value;
        mmPoints: Result := MulDiv(Value, 72, 1000);
        mmTWIPS: Result := MulDiv(Value, 1440, 1000);
        mmPixel: Result := MulDiv(Value, DPI, 1000);
      end;
    mmPoints:
      case OutUnits of
        mmLoMetric: Result := MulDiv(Value, 254, 72);
        mmHiMetric: Result := MulDiv(Value, 2540, 72);
        mmLoEnglish: Result := MulDiv(Value, 100, 72);
        mmHiEnglish: Result := MulDiv(Value, 1000, 72);
        mmPoints: Result := Value;
        mmTWIPS: Result := Value * 20;
        mmPixel: Result := MulDiv(Value, DPI, 72);
      end;
    mmTWIPS:
      case OutUnits of
        mmLoMetric: Result := MulDiv(Value, 254, 1440);
        mmHiMetric: Result := MulDiv(Value, 2540, 1440);
        mmLoEnglish: Result := MulDiv(Value, 100, 1440);
        mmHiEnglish: Result := MulDiv(Value, 1000, 1440);
        mmPoints: Result := Value div 20;
        mmTWIPS: Result := Value;
        mmPixel: Result := MulDiv(Value, DPI, 1440);
      end;
    mmPixel:
      case OutUnits of
        mmLoMetric: Result := MulDiv(Value, 254, DPI);
        mmHiMetric: Result := MulDiv(Value, 2540, DPI);
        mmLoEnglish: Result := MulDiv(Value, 100, DPI);
        mmHiEnglish: Result := MulDiv(Value, 1000, DPI);
        mmPoints: Result := MulDiv(Value, 72, DPI);
        mmTWIPS: Result := MulDiv(Value, 1440, DPI);
        mmPixel: Result := MulDiv(Value, DPI, Screen.PixelsPerInch);
      end;
  end;
end;

constructor TPrintPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  Align := alClient;
  TabStop := True;
  ParentFont := False;
  Font.Name := 'Arial';
  Font.Size := 8;
  FAborted := False;
  FState := psReady;
  FPaperType := pA4;
  FOrientation := poPortrait;
  FZoomSavePos := True;
  FZoomState := zsZoomToFit;
  FZoom := 100;
  FZoomMin := 10;
  FZoomMax := 500;
  FZoomStep := 10;
  SetUnits(mmHiMetric);
  with PaperSizes[FPaperType] do
    FPageExt := ConvertXY(Width, Height, Units, FUnits);
  CalculateMetafileSize;
  FPages := TMetafileList.Create;
  FPages.OnChange := PagesChanged;
  FPages.OnCurrentChange := PageChanged;
  FPaperViewOptions := TPaperPreviewOptions.Create;
  FPaperViewOptions.OnChange := PaperViewOptionsChanged;
  FPaperView := TPaperPreview.Create(Self);
  with FPaperView do
  begin
    Parent := Self;
    TabStop := False;
    Visible := False;
    OnPaint := PaintPage;
    OnClick := PaperClick;
    OnDblClick := PaperDblClick;
    OnMouseDown := PaperMouseDown;
    OnMouseMove := PaperMouseMove;
    OnMouseUp := PaperMouseUp;
  end;
  FPaperViewOptions.AssignTo(FPaperView);
end;

destructor TPrintPreview.Destroy;
begin
  FPages.Free;
  FPaperView.Free;
  FPaperViewOptions.Free;
  if Assigned(AnnotationMetafile) then
    AnnotationMetafile.Free;
  if Assigned(BackgroundMetafile) then
    BackgroundMetafile.Free;
  if AutoFormName <> '' then
    RemoveForm(AutoFormName);
  inherited Destroy;
end;

procedure TPrintPreview.Loaded;
begin
  inherited Loaded;
  CalculateMetafileSize;
  UpdateZoom;
end;

function TPrintPreview.ConvertX(Value: Integer; InUnits, OutUnits: TUnits): Integer;
begin
  Result := ConvertXY(Value, 0, InUnits, OutUnits).X;
end;

function TPrintPreview.ConvertY(Value: Integer; InUnits, OutUnits: TUnits): Integer;
begin
  Result := ConvertXY(0, Value, InUnits, OutUnits).Y;
end;

function TPrintPreview.ConvertXY(X, Y: Integer; InUnits, OutUnits: TUnits): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
  ConvertPoints(Result, 1, InUnits, OutUnits);
end;

procedure TPrintPreview.ConvertPoints(var Points; NumPoints: Integer;
  InUnits, OutUnits: TUnits);
var
  pPoints: PPoint;
begin
  pPoints := @Points;
  while NumPoints > 0 do
  begin
    with pPoints^ do
    begin
      X := ConvertUnits(X, Screen.PixelsPerInch, InUnits, OutUnits);
      Y := ConvertUnits(Y, Screen.PixelsPerInch, InUnits, OutUnits);
    end;
    Inc(pPoints);
    Dec(NumPoints);
  end;
end;

function TPrintPreview.ClientToPaper(const Pt: TPoint): TPoint;
begin
  Result := Pt;
  MapWindowPoints(Handle, FPaperView.Handle, Result, 1);
  Result.X := MulDiv(Result.X - FPaperViewOptions.BorderWidth, 100, FZoom);
  Result.Y := MulDiv(Result.Y - FPaperViewOptions.BorderWidth, 100, FZoom);
  Result.X := ConvertUnits(Result.X, Screen.PixelsPerInch, mmPixel, FUnits);
  Result.Y := ConvertUnits(Result.Y, Screen.PixelsPerInch, mmPixel, FUnits);
end;

function TPrintPreview.PaperToClient(const Pt: TPoint): TPoint;
begin
  Result.X := ConvertUnits(Pt.X, Screen.PixelsPerInch, FUnits, mmPixel);
  Result.Y := ConvertUnits(Pt.Y, Screen.PixelsPerInch, FUnits, mmPixel);
  Result.X := MulDiv(Result.X, FZoom, 100) + FPaperViewOptions.BorderWidth;
  Result.Y := MulDiv(Result.Y, FZoom, 100) + FPaperViewOptions.BorderWidth;
  MapWindowPoints(FPaperView.Handle, Handle, Result, 1);
end;

function TPrintPreview.PaintGraphic(X, Y: Integer; Graphic: TGraphic): TPoint;
var
  Rect: TRect;
begin
  Rect.Left := X;
  Rect.Top := Y;
  Rect.Right := X + ConvertUnits(Graphic.Width, Screen.PixelsPerInch, mmPixel, FUnits);
  Rect.Bottom := Y + ConvertUnits(Graphic.Height, Screen.PixelsPerInch, mmPixel, FUnits);
  Result := PaintGraphicEx(Rect, Graphic, False, False, False).BottomRight;
end;

function TPrintPreview.PaintGraphicEx(const Rect: TRect; Graphic: TGraphic;
  Proportinal, ShrinkOnly, Center: Boolean): TRect;
var
  gW, gH: Integer;
  rW, rH: Integer;
  W, H: Integer;
begin
  gW := ConvertUnits(Graphic.Width, Screen.PixelsPerInch, mmPixel, FUnits);
  gH := ConvertUnits(Graphic.Height, Screen.PixelsPerInch, mmPixel, FUnits);
  rW := Rect.Right - Rect.Left;
  rH := Rect.Bottom - Rect.Top;
  if not ShrinkOnly or (gW > rW) or (gH > rH) then
  begin
    if Proportinal then
    begin
      if (rW / gW) < (rH / gH) then
      begin
        H := MulDiv(gH, rW, gW);
        W := rW;
      end
      else
      begin
        W := MulDiv(gW, rH, gH);
        H := rH;
      end;
    end
    else
    begin
      W := rW;
      H := rH;
    end;
  end
  else
  begin
    W := gW;
    H := gH;
  end;
  if Center then
  begin
    Result.Left := Rect.Left + (rW - W) div 2;
    Result.Top := Rect.Top + (rH - H) div 2;
  end
  else
    Result.TopLeft := Rect.TopLeft;
  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;
  StretchDrawGraphic(Canvas, Result, Graphic);
end;

//rmk
function TPrintPreview.PaintGraphicEx2(const Rect: TRect; Graphic: TGraphic;
      VertAlign: TVertAlign; HorzAlign: THorzAlign): TRect;
var
  gW, gH: Integer;
  rW, rH: Integer;
  W, H: Integer;
begin
  gW := ConvertUnits(Graphic.Width, Screen.PixelsPerInch, mmPixel, FUnits);
  gH := ConvertUnits(Graphic.Height, Screen.PixelsPerInch, mmPixel, FUnits);
  rW := Rect.Right - Rect.Left;
  rH := Rect.Bottom - Rect.Top;

  if (gW > rW) or (gH > rH) then
  begin
    if (rW / gW) < (rH / gH) then
    begin
      H := MulDiv(gH, rW, gW);
      W := rW;
    end
    else
    begin
      W := MulDiv(gW, rH, gH);
      H := rH;
    end;
  end
  else
  begin
    W := gW;
    H := gH;
  end;

  Case VertAlign of
    vaTop   : Result.Top := Rect.Top;
    vaCenter: Result.Top := Rect.Top + (rH - H) div 2;
    vaBottom: Result.Top := Rect.Bottom - H;
  else
    Result.Top := Rect.Top + (rH - H) div 2;
  end;

  Case HorzAlign of
    haLeft  : Result.Left := Rect.Left;
    haCenter: Result.Left := Rect.Left + (rW - W) div 2;
    haRight : Result.Left := Rect.Right - W;
  else
    Result.Left := Rect.Left + (rW - W) div 2;
  end;

  Result.Right := Result.Left + W;
  Result.Bottom := Result.Top + H;

  StretchDrawGraphic(Canvas, Result, Graphic);
end;

function TPrintPreview.PaintWinControl(X, Y: Integer;
  WinControl: TWinControl): TPoint;
var
  Rect: TRect;
begin
  Rect.Left := X;
  Rect.Top := Y;
  Rect.Right := X + ConvertUnits(WinControl.Width, Screen.PixelsPerInch, mmPixel, FUnits);
  Rect.Bottom := Y + ConvertUnits(WinControl.Height, Screen.PixelsPerInch, mmPixel, FUnits);
  Result := PaintWinControlEx(Rect, WinControl, False, False, False).BottomRight;
end;

function TPrintPreview.PaintWinControlEx(const Rect: TRect;
  WinControl: TWinControl; Proportinal, ShrinkOnly, Center: Boolean): TRect;
var
  Metafile: TMetafile;
  MetaCanvas: TCanvas;
begin
  Metafile := TMetafile.Create;
  try
    Metafile.Width := WinControl.Width;
    Metafile.Height := WinControl.Height;
    MetaCanvas := TMetafileCanvas.Create(Metafile, 0);
    try
      MetaCanvas.Lock;
      try
        WinControl.PaintTo(MetaCanvas.Handle, 0, 0);
      finally
        MetaCanvas.Unlock;
      end;
    finally
      MetaCanvas.Free;
    end;
    Result := PaintGraphicEx(Rect, Metafile, Proportinal, ShrinkOnly, Center);
  finally
    Metafile.Free;
  end;
end;

function TPrintPreview.PaintRichText(const Rect: TRect;
  RichEdit: TCustomRichEdit; MaxPages: Integer; pOffset: PInteger): Integer;
var
  Range: TFormatRange;
  RectTWIPS: TRect;
  SaveIndex: Integer;
  MaxLen: Integer;
  TextLenEx: TGetTextLengthEx;
begin
  Result := 0;
  RectTWIPS := Rect;
  ConvertPoints(RectTWIPS, 2, FUnits, mmTWIPS);
  FillChar(Range, SizeOf(TFormatRange), 0);
  if pOffset = nil then
    Range.chrg.cpMin := 0
  else
    Range.chrg.cpMin := pOffset^;
  TextLenEx.flags := GTL_DEFAULT;
  TextLenEx.codepage := CP_UTF8;
  MaxLen := SendMessage(RichEdit.Handle, EM_GETTEXTLENGTHEX, Integer(@TextLenEx), 0);
  SaveIndex := SaveDC(FPageCanvas.Handle);
  try
    SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0);
    repeat
      if Result > 0  then
      begin
        RestoreDC(FPageCanvas.Handle, SaveIndex);
        NewPage;
        SaveIndex := SaveDC(FPageCanvas.Handle);
      end;
      Range.chrg.cpMax := -1;
      Range.rc := RectTWIPS;
      Range.rcPage := RectTWIPS;
      Range.hdc := FPageCanvas.Handle;
      SetMapMode(FPageCanvas.Handle, MM_TEXT);
      Range.chrg.cpMin := SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, Integer(@Range));
      SendMessage(RichEdit.Handle, EM_DISPLAYBAND, 0, Integer(@Range.rc));
      if Range.chrg.cpMin <> -1 then
        Inc(Result);
    until (Range.chrg.cpMin >= MaxLen) or (Range.chrg.cpMin = -1) or
          ((MaxPages > 0) and (Result >= MaxPages));
  finally
    SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0);
    RestoreDC(FPageCanvas.Handle, SaveIndex);
  end;
  if pOffset <> nil then
    if Range.chrg.cpMin < MaxLen then
      pOffset^ := Range.chrg.cpMin
    else
      pOffset^ := -1;
end;

function TPrintPreview.GetRichTextRect(var Rect: TRect;
  RichEdit: TCustomRichEdit; pOffset: PInteger): Integer;
var
  Range: TFormatRange;
  RectTWIPS: TRect;
  SaveIndex: Integer;
  MaxLen: Integer;
  TextLenEx: TGetTextLengthEx;
begin
  RectTWIPS := Rect;
  ConvertPoints(RectTWIPS, 2, FUnits, mmTWIPS);
  FillChar(Range, SizeOf(TFormatRange), 0);
  Range.rc := RectTWIPS;
  Range.rcPage := RectTWIPS;
  Range.hdc := FPageCanvas.Handle;
  Range.chrg.cpMax := -1;
  if pOffset = nil then
    Range.chrg.cpMin := 0
  else
    Range.chrg.cpMin := pOffset^;
  SaveIndex := SaveDC(FPageCanvas.Handle);
  try
    SetMapMode(FPageCanvas.Handle, MM_TEXT);
    SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0);
    Range.chrg.cpMin := SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, Integer(@Range));
    if Range.chrg.cpMin = -1 then
      Rect.Bottom := Rect.Top
    else
      Rect.Bottom := ConvertY(Range.rc.Bottom, mmTWIPS, FUnits);
  finally
    SendMessage(RichEdit.Handle, EM_FORMATRANGE, 0, 0);
    RestoreDC(FPageCanvas.Handle, SaveIndex);
  end;
  if pOffset <> nil then
  begin
    TextLenEx.flags := GTL_DEFAULT;
    TextLenEx.codepage := CP_UTF8;
    MaxLen := SendMessage(RichEdit.Handle, EM_GETTEXTLENGTHEX, Integer(@TextLenEx), 0);
    if Range.chrg.cpMin < MaxLen then
      pOffset^ := Range.chrg.cpMin
    else
      pOffset^ := -1;
  end;
  Result := Rect.Bottom;
end;

procedure TPrintPreview.CNKeyDown(var Message: TWMKey);
var
  Key: Word;
  Shift: TShiftState;
begin
  with Message do
  begin
    Key := CharCode;
    Shift := KeyDataToShiftState(KeyData);
  end;
  if (Key = VK_HOME) and (Shift = []) then
    with HorzScrollbar do Position := 0
  else if (Key = VK_HOME) and (Shift = [ssCtrl]) then
    with VertScrollbar do Position := 0
  else if (Key = VK_END) and (Shift = []) then
    with HorzScrollbar do Position := Range
  else if (Key = VK_END) and (Shift = [ssCtrl]) then
    with VertScrollbar do Position := Range
  else if (Key = VK_LEFT) and (Shift = [ssShift]) then
    with HorzScrollbar do Position := Position - 1
  else if (Key = VK_LEFT) and (Shift = []) then
    with HorzScrollbar do Position := Position - Increment
  else if (Key = VK_LEFT) and (Shift = [ssCtrl]) then
    with HorzScrollbar do Position := Position - ClientWidth
  else if (Key = VK_RIGHT) and (Shift = [ssShift]) then
    with HorzScrollbar do Position := Position + 1
  else if (Key = VK_RIGHT) and (Shift = []) then
    with HorzScrollbar do Position := Position + Increment
  else if (Key = VK_RIGHT) and (Shift = [ssCtrl]) then
    with HorzScrollbar do Position := Position + ClientWidth
  else if (Key = VK_UP) and (Shift = [ssShift]) then
    with VertScrollbar do Position := Position - 1
  else if (Key = VK_UP) and (Shift = []) then
    with VertScrollbar do Position := Position - Increment
  else if (Key = VK_UP) and (Shift = [ssCtrl]) then
    with VertScrollbar do Position := Position - ClientHeight
  else if (Key = VK_DOWN) and (Shift = [ssShift]) then
    with VertScrollbar do Position := Position + 1
  else if (Key = VK_DOWN) and (Shift = []) then
    with VertScrollbar do Position := Position + Increment
  else if (Key = VK_DOWN) and (Shift = [ssCtrl]) then
    with VertScrollbar do Position := Position + ClientHeight
  else if (Key = VK_NEXT) and (Shift = []) then
    CurrentPage := CurrentPage + 1
  else if (Key = VK_NEXT) and (Shift = [ssCtrl]) then
    CurrentPage := TotalPages
  else if (Key = VK_PRIOR) and (Shift = []) then
    CurrentPage := CurrentPage - 1
  else if (Key = VK_PRIOR) and (Shift = [ssCtrl]) then
    CurrentPage := 1
  else if (Key = VK_ADD) and (Shift = []) then
    Zoom := Zoom + ZoomStep
  else if (Key = VK_SUBTRACT) and (Shift = []) then
    Zoom := Zoom - ZoomStep
  else
    inherited;
end;

procedure TPrintPreview.WMMouseWheel(var Message: TMessage);
var
  IsNeg: Boolean;
  Rect: TRect;
  Pt: TPoint;
begin
  GetWindowRect(WindowHandle, Rect);
  Pt.X := Message.LParamLo;
  Pt.Y := Message.LParamHi;
  if PtInRect(Rect, Pt) then
  begin
    Message.Result := 1;
    Inc(WheelAccumulator, SmallInt(Message.WParamHi));
    while Abs(WheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := WheelAccumulator < 0;
      WheelAccumulator := Abs(WheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        WheelAccumulator := -WheelAccumulator;
        case LoWord(Message.WParam) of
          MK_CONTROL: Zoom := Zoom - ZoomStep;
          MK_SHIFT, MK_MBUTTON: CurrentPage := CurrentPage + 1;
          0: with VertScrollbar do Position := Position + Increment;
        else
          Message.Result := 0;
        end;
      end
      else
      begin
        case LoWord(Message.WParam) of
          MK_CONTROL: Zoom := Zoom + ZoomStep;
          MK_SHIFT, MK_MBUTTON: CurrentPage := CurrentPage - 1;
          0: with VertScrollbar do Position := Position - Increment;
        else
          Message.Result := 0;
        end;
      end;
    end;
  end;
end;

procedure TPrintPreview.PaperClick(Sender: TObject);
begin
  Click;
end;

procedure TPrintPreview.PaperDblClick(Sender: TObject);
begin
  DblClick;
end;

procedure TPrintPreview.PaperMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  if not Focused and Enabled then SetFocus;
  //pvg begin
  if (Sender = FPaperView) and (FCanScrollHorz or FCanScrollVert) then
  begin
    FIsDragging := True;
    FPaperView.Cursor := FPaperViewOptions.GrabCursor;
    FPaperView.Perform(WM_SETCURSOR, FPaperView.Handle, HTCLIENT);
  end;
  //pvg end
  Pt.X := X;
  Pt.Y := Y;
  FOldMousePos := Pt;
  MapWindowPoints(FPaperView.Handle, Handle, Pt, 1);
  MouseDown(Button, Shift, Pt.X, Pt.Y);
end;

procedure TPrintPreview.PaperMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Delta: TPoint;
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  MapWindowPoints(FPaperView.Handle, Handle, Pt, 1);
  MouseMove(Shift, Pt.X, Pt.Y);
  if ssLeft in Shift then
  begin
    if FCanScrollHorz then
    begin
      Delta.X := X - FOldMousePos.X;
      if not (AutoScroll and HorzScrollBar.Visible) then
      begin
        if FPaperView.Left + Delta.X < ClientWidth - HorzScrollBar.Margin - FPaperView.Width then
          Delta.X := ClientWidth - HorzScrollBar.Margin - FPaperView.Width - FPaperView.Left
        else if FPaperView.Left + Delta.X > HorzScrollBar.Margin then
          Delta.X := HorzScrollBar.Margin - FPaperView.Left;
        FPaperView.Left := FPaperView.Left + Delta.X;
      end
      else
        HorzScrollBar.Position := HorzScrollBar.Position - Delta.X;
    end;
    if FCanScrollVert then
    begin
      Delta.Y := Y - FOldMousePos.Y;
      if not (AutoScroll and VertScrollBar.Visible) then
      begin
        if FPaperView.Top + Delta.Y < ClientHeight - VertScrollBar.Margin - FPaperView.Height then
          Delta.Y := ClientHeight - VertScrollBar.Margin - FPaperView.Height - FPaperView.Top
        else if FPaperView.Top + Delta.Y > VertScrollBar.Margin then
          Delta.Y := VertScrollBar.Margin - FPaperView.Top;
        FPaperView.Top := FPaperView.Top + Delta.Y;
      end
      else
        VertScrollBar.Position := VertScrollBar.Position - Delta.Y;
    end;
  end;
end;

procedure TPrintPreview.PaperMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  MapWindowPoints(FPaperView.Handle, Handle, Pt, 1);
  MouseUp(Button, Shift, Pt.X, Pt.Y);
  //pvg begin
  if FIsDragging then
  begin
    FIsDragging := False;
    FPaperView.Cursor := FPaperViewOptions.DragCursor;
  end;
  //pvg end
end;

procedure TPrintPreview.SetPrinterOptions;
var
  DeviceMode: THandle;
  DevMode: PDeviceMode;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  DriverInfo2: PDriverInfo2;
  DriverInfo2Size: DWORD;
  hPrinter: THandle;
  PaperSize: TPoint;
  ActiveForm: String;
begin
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    DevMode := PDevMode(GlobalLock(DeviceMode));
    try
      with DevMode^ do
      begin
        dmFields := dmFields and not
          (DM_FORMNAME or DM_PAPERSIZE or DM_PAPERWIDTH or DM_PAPERLENGTH);
        ActiveForm := FormName;
        if ActiveForm <> '' then
        begin
          dmFields := dmFields or DM_FORMNAME;
          StrPLCopy(dmFormName, ActiveForm, CCHFORMNAME);
        end;
        if IsCustomPaper then
        begin
          PaperSize := ConvertXY(FPageExt.X, FPageExt.Y, FUnits, mmLoMetric);
          if FOrientation = poLandscape then
            SwapValues(PaperSize.X, PaperSize.Y);
          dmFields := dmFields or DM_PAPERSIZE;
          dmPaperSize := DMPAPER_USER;
          dmFields := dmFields or DM_PAPERWIDTH;
          dmPaperWidth := PaperSize.X;
          dmFields := dmFields or DM_PAPERLENGTH;
          dmPaperLength := PaperSize.Y;
        end
        else
        begin
          dmFields := dmFields or DM_PAPERSIZE;
          dmPaperSize := PaperSizes[FPaperType].ID;
        end;
        dmFields := dmFields or DM_ORIENTATION;
        case FOrientation of
          poPortrait: dmOrientation := DMORIENT_PORTRAIT;
          poLandscape: dmOrientation := DMORIENT_LANDSCAPE;
        end;
      end;
    finally
      GlobalUnlock(DeviceMode);
    end;
    ResetDC(Printer.Handle, DevMode^);
    OpenPrinter(Device, hPrinter, nil);
    try
      GetPrinterDriver(hPrinter, nil, 2, nil, 0, DriverInfo2Size);
      GetMem(DriverInfo2, DriverInfo2Size);
      try
        GetPrinterDriver(hPrinter, nil, 2, DriverInfo2, DriverInfo2Size, DriverInfo2Size);
        StrPCopy(Driver, ExtractFileName(StrPas(DriverInfo2^.PDriverPath)));
      finally
        FreeMem(DriverInfo2, DriverInfo2Size);
      end;
    finally
      ClosePrinter(hPrinter);
    end;
    Printer.SetPrinter(Device, Driver, Port, DeviceMode);
  end;
end;

procedure TPrintPreview.GetPrinterOptions;
var
  Size: TSize;
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
begin
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    with PDevMode(GlobalLock(DeviceMode))^ do
      try
        if (dmFields and DM_ORIENTATION) = DM_ORIENTATION then
          case dmOrientation of
            DMORIENT_PORTRAIT: SetOrientation(poPortrait);
            DMORIENT_LANDSCAPE: SetOrientation(poLandscape);
          end;
        if (dmFields and DM_PAPERSIZE) = DM_PAPERSIZE then
          SetPaperTypeByID(dmPaperSize)
        else
          SetPaperType(pCustom);
        if IsCustomPaper then
        begin
          Size.cx := ConvertUnits(GetDeviceCaps(Printer.Handle, PHYSICALWIDTH),
            GetDeviceCaps(Printer.Handle, LOGPIXELSX), mmPixel, Units);
          Size.cy := ConvertUnits(GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT),
            GetDeviceCaps(Printer.Handle, LOGPIXELSY), mmPixel, Units);
          if Orientation = poPortrait then
            SetPaperSize(Size.cx, Size.cy)
          else
            SetPaperSize(Size.cy, Size.cx);
        end;
        if (dmFields and DM_FORMNAME) = DM_FORMNAME then
          FFormName := dmFormName;
      finally
        GlobalUnlock(DeviceMode);
      end;
  end;
end;

function TPrintPreview.FetchFormNames(FormNames: TStrings): Boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  hPrinter: THandle;
  pFormsInfo, pfi: PFormInfo1;
  BytesNeeded: DWORD;
  FormCount: DWORD;
  I: Integer;
begin
  Result := False;
  FormNames.BeginUpdate;
  try
    FormNames.Clear;
    if PrinterInstalled then
    begin
      Printer.GetPrinter(Device, Driver, Port, DeviceMode);
      OpenPrinter(Device, hPrinter, nil);
      try
        EnumForms(hPrinter, 1, nil, 0, BytesNeeded, FormCount);
        BytesNeeded := 0;
        EnumForms(hPrinter, 1, nil, 0, BytesNeeded, FormCount);
        if BytesNeeded > 0 then
        begin
          FormCount := BytesNeeded div SizeOf(TFormInfo1);
          GetMem(pFormsInfo, BytesNeeded);
          try
            if EnumForms(hPrinter, 1, pFormsInfo, BytesNeeded, BytesNeeded, FormCount) then
            begin
              Result := True;
              pfi := pFormsInfo;
              for I := 0 to FormCount - 1 do
              begin
                if (pfi^.Size.CX > 10) and (pfi^.Size.CY > 10) then
                  FormNames.Add(pfi^.pName);
                Inc(pfi);
              end;
            end;
          finally
            FreeMem(pFormsInfo);
          end;
        end;
      finally
        ClosePrinter(hPrinter);
      end;
    end;
  finally
    FormNames.EndUpdate;
  end;
end;

function TPrintPreview.GetFormSize(const AFormName: String;
  out FormWidth, FormHeight: Integer): Boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  hPrinter: THandle;
  pFormInfo: PFormInfo1;
  BytesNeeded: DWORD;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      BytesNeeded := 0;
      GetForm(hPrinter, PChar(AFormName), 1, nil, 0, BytesNeeded);
      if BytesNeeded > 0 then
      begin
        GetMem(pFormInfo, BytesNeeded);
        try
          if GetForm(hPrinter, PChar(AFormName), 1, pFormInfo, BytesNeeded, BytesNeeded) then
          begin
            with ConvertXY(pFormInfo.Size.CX div 10, pFormInfo.Size.CY div 10, mmHiMetric, Units) do
            begin
              FormWidth := X;
              FormHeight := Y;
            end;
            Result := True;
          end;
        finally
          FreeMem(pFormInfo);
        end;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

function TPrintPreview.AddNewForm(const AFormName: String;
  FormWidth, FormHeight: DWORD): Boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  hPrinter: THandle;
  FormInfo: TFormInfo1;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      with FormInfo do
      begin
        Flags := 0;
        pName := PChar(AFormName);
        with ConvertXY(FormWidth, FormHeight, Units, mmHiMetric) do
        begin
          Size.CX := X * 10;
          Size.CY := Y * 10;
        end;
        SetRect(ImageableArea, 0, 0, Size.CX, Size.CY);
      end;
      Result := AddForm(hPrinter, 1, @FormInfo);
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

function TPrintPreview.RemoveForm(const AFormName: String): Boolean;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  hPrinter: THandle;
begin
  Result := False;
  if PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      if DeleteForm(hPrinter, PChar(AFormName)) then
      begin
        if CompareText(FFormName, AFormName) = 0 then
          FFormName := '';
        if CompareText(FAutoFormName, AFormName) = 0 then
        begin
          if Assigned(FOnAutoCustomForm) and not (csDestroying in ComponentState) then
            FOnAutoCustomForm(Self, FAutoFormName, opRemove);
          FAutoFormName := '';
        end;
        Result := True;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
  end;
end;

procedure TPrintPreview.CheckForAutoCustomForm;
var
  NewFormName: String;
  FormWidth, FormHeight: Integer;
begin
  if (FAutoFormName <> '') and (CompareText(FFormName, FAutoFormName) = 0) then
    RemoveForm(FAutoFormName);
  if FFormName = '' then
  begin
    FormWidth := PaperWidth;
    FormHeight := PaperHeight;
    if Orientation = poLandscape then
      SwapValues(FormWidth, FormHeight);
    with ConvertXY(FormWidth, FormHeight, Units, mmHiMetric) do
      NewFormName := Format('%umm x %umm', [X div 100, Y div 100]);
    if AddNewForm(NewFormName, FormWidth, FormHeight) then
    begin
      FFormName := NewFormName;
      FAutoFormName := NewFormName;
      if Assigned(FOnAutoCustomForm) then
        FOnAutoCustomForm(Self, FAutoFormName, opInsert);
    end;
  end;
end;

function TPrintPreview.GetFormName: String;
var
  DeviceMode: THandle;
  Device, Driver, Port: array[0..MAX_PATH] of Char;
  hPrinter: THandle;
  PaperSize: TPoint;
  pForms, pf: PFormInfo1;
  BytesNeeded: DWORD;
  FormCount: DWORD;
  I: Integer;
begin
  if (FFormName = '') and PrinterInstalled then
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    OpenPrinter(Device, hPrinter, nil);
    try
      BytesNeeded := 0;
      EnumForms(hPrinter, 1, nil, 0, BytesNeeded, FormCount);
      if BytesNeeded > 0 then
      begin
        FormCount := BytesNeeded div SizeOf(TFormInfo1);
        GetMem(pForms, BytesNeeded);
        try
          if EnumForms(hPrinter, 1, pForms, BytesNeeded, BytesNeeded, FormCount) then
          begin
            PaperSize := ConvertXY(FPageExt.X, FPageExt.Y, FUnits, mmLoMetric);
            if Orientation = poLandscape then SwapValues(PaperSize.X, PaperSize.Y);
            pf := pForms;
            for I := 0 to FormCount - 1 do
            begin
              if ((pf^.Size.CX div 100) = PaperSize.X) and
                 ((pf^.Size.CY div 100) = PaperSize.Y) then
              begin
                FFormName := pf^.pName;
                Break;
              end;
              Inc(pf);
            end;
          end;
        finally
          FreeMem(pForms);
        end;
      end;
    finally
      ClosePrinter(hPrinter);
    end;
    CheckForAutoCustomForm;
  end;
  Result := FFormName;
end;

procedure TPrintPreview.SetFormName(const Value: String);
var
  FormWidth, FormHeight: Integer;
begin
  if (CompareText(FFormName, Value) <> 0) and (FState = psReady) and
      GetFormSize(Value, FormWidth, FormHeight) and
     (FormWidth <> 0) and (FormHeight <> 0) then
  begin
    FFormName := Value;
    if Orientation = poPortrait then
      SetPaperSize(FormWidth, FormHeight)
    else
      SetPaperSize(FormHeight, FormWidth);
  end;
end;

function TPrintPreview.FindPaperType(APaperWidth, APaperHeight: Integer;
  InUnits: TUnits): TPaperType;
var
  Paper: TPaperType;
begin
  Result := pCustom;
  for Paper := Low(TPaperType) to High(TPaperType) do
    with ConvertXY(APaperWidth, APaperHeight, InUnits, PaperSizes[Paper].Units) do
      if (PaperSizes[Paper].Width = X) and (PaperSizes[Paper].Height = Y) then
      begin
        Result := Paper;
        Exit;
      end;
end;

procedure TPrintPreview.Resize;
begin
  inherited Resize;
  UpdateZoom;
end;

function TPrintPreview.CalculateViewSize(const Space: TPoint): TPoint;
begin
  with FPaperView do
  begin
    case FZoomState of
      zsZoomOther:
      begin
        Result.X := ActualSize(MulDiv(FDeviceExt.X, FZoom, 100));
        Result.Y := ActualSize(MulDiv(FDeviceExt.Y, FZoom, 100));
      end;
      zsZoomToWidth:
      begin
        Result.X := Space.X;
        Result.Y := ActualSize(MulDiv(LogicalSize(Result.X), FDeviceExt.Y, FDeviceExt.X));
      end;
      zsZoomToHeight:
      begin
        Result.Y := Space.Y;
        Result.X := ActualSize(MulDiv(LogicalSize(Result.Y), FDeviceExt.X, FDeviceExt.Y));
      end;
      zsZoomToFit:
      begin
        if (FDeviceExt.Y / FDeviceExt.X) < (Space.Y / Space.X) then
        begin
          Result.X := Space.X;
          Result.Y := ActualSize(MulDiv(LogicalSize(Result.X), FDeviceExt.Y, FDeviceExt.X));
        end
        else
        begin
          Result.Y := Space.Y;
          Result.X := ActualSize(MulDiv(LogicalSize(Result.Y), FDeviceExt.X, FDeviceExt.Y));
        end;
      end;
    end;
    if FZoomState <> zsZoomOther then
      FZoom := Round((100 * LogicalSize(Result.Y)) / FDeviceExt.Y);
  end;
end;

{$WARNINGS OFF}
procedure TPrintPreview.UpdateZoom;
var
  Space: TPoint;
  Percent: TPoint;
  ViewPos: TPoint;
  ViewSize: TPoint;
  OldZoom: Integer;
begin
  if (csLoading in ComponentState) or
     ((FPages.Count = 0) and not (csDesigning in ComponentState))
  then
    Exit;

  OldZoom := FZoom;

  Space.X := ClientWidth - 2 * HorzScrollBar.Margin;
  Space.Y := ClientHeight - 2 * VertScrollBar.Margin;

  if FZoomSavePos and (FCurrentPage <> 0) then
  begin
    Percent.X := MulDiv(HorzScrollbar.Position, 100, HorzScrollBar.Range - Space.X);
    if Percent.X < 0 then Percent.X := 0;
    Percent.Y := MulDiv(VertScrollbar.Position, 100, VertScrollbar.Range - Space.Y);
    if Percent.Y < 0 then Percent.Y := 0;
  end;

  if AutoScroll then
  begin
    {$IFNDEF COMPILER4_UP}
    if HorzScrollBar.Visible and (GetWindowLong(Handle, GWL_STYLE) and SB_HORZ <> 0) then
    {$ELSE}
    if HorzScrollBar.IsScrollBarVisible then
    {$ENDIF}
      Inc(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
    {$IFNDEF COMPILER4_UP}
    if VertScrollBar.Visible and (GetWindowLong(Handle, GWL_STYLE) and SB_VERT <> 0) then
    {$ELSE}
    if VertScrollBar.IsScrollBarVisible then
    {$ENDIF}
      Inc(Space.X, GetSystemMetrics(SM_CXVSCROLL));
  end;

  DisableAutoRange;

  try

    HorzScrollbar.Position := 0;
    VertScrollbar.Position := 0;

    ViewSize := CalculateViewSize(Space);

    FCanScrollHorz := (ViewSize.X > Space.X);
    FCanScrollVert := (ViewSize.Y > Space.Y);

    if AutoScroll then
    begin
      if FCanScrollHorz then
      begin
         Dec(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
         FCanScrollVert := (FPaperView.Height > Space.Y);
         if FCanScrollVert then
           Dec(Space.X, GetSystemMetrics(SM_CXVSCROLL));
         ViewSize := CalculateViewSize(Space);
      end
      else if FCanScrollVert then
      begin
         Dec(Space.X, GetSystemMetrics(SM_CXVSCROLL));
         FCanScrollHorz := (FPaperView.Width > Space.X);
         if FCanScrollHorz then
           Dec(Space.Y, GetSystemMetrics(SM_CYHSCROLL));
         ViewSize := CalculateViewSize(Space);
      end;
    end;

    ViewPos.X := HorzScrollBar.Margin;
    if not FCanScrollHorz then
      Inc(ViewPos.X, (Space.X - ViewSize.X) div 2);

    ViewPos.Y := VertScrollBar.Margin;
    if not FCanScrollVert then
      Inc(ViewPos.Y, (Space.Y - ViewSize.Y) div 2);

    FPaperView.SetBounds(ViewPos.X, ViewPos.Y, ViewSize.X, ViewSize.Y);

  finally
    EnableAutoRange;
  end;

  if FCurrentPage <> 0 then
  begin
    if FZoomSavePos and FCanScrollHorz then
      HorzScrollbar.Position := MulDiv(Percent.X, HorzScrollBar.Range - Space.X, 100);
    if FZoomSavePos and FCanScrollVert then
       VertScrollbar.Position := MulDiv(Percent.Y, VertScrollbar.Range - Space.Y, 100);
  end;

  FIsDragging := False;
  if FCanScrollHorz or FCanScrollVert then
    FPaperView.Cursor := FPaperViewOptions.DragCursor
  else
    FPaperView.Cursor := FPaperViewOptions.Cursor;

  if (OldZoom <> FZoom) and Assigned(FOnZoomChange) then
    FOnZoomChange(Self);
end;
{$WARNINGS ON}

procedure TPrintPreview.PaintPage(Sender: TObject; Canvas: TCanvas;
  const PageRect: TRect);
begin
  if (FCurrentPage >= 1) and (FCurrentPage <= TotalPages) then
  begin
    if Assigned(BackgroundMetafile) then
      Canvas.StretchDraw(PageRect, BackgroundMetafile);
    if gsPreview in FGrayscale then
      StretchDrawGrayscale(Canvas, PageRect, FPages[FCurrentPage-1])
    else
      Canvas.StretchDraw(PageRect, FPages[FCurrentPage-1]);
    if Assigned(AnnotationMetafile) then
      Canvas.StretchDraw(PageRect, AnnotationMetafile);
  end;
end;

procedure TPrintPreview.PaperViewOptionsChanged(Sender: TObject);
begin
  FPaperViewOptions.AssignTo(FPaperView);
  UpdateZoom;
end;

procedure TPrintPreview.PagesChanged(Sender: TObject);
begin
  if TotalPages = 0 then
  begin
    FCurrentPage := 0;
    FPaperView.Visible := False;
  end
  else if FCurrentPage = 0 then
  begin
    UpdateZoom;
    FCurrentPage := 1;
    FPaperView.Visible := True;
  end;
  DoBackground(FCurrentPage);
  DoAnnotation(FCurrentPage);
  FPaperView.Refresh;
  UpdateThumbnailViews(True);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPrintPreview.PageChanged(Sender: TObject; PageIndex: Integer);
begin
  if PageIndex = (FCurrentPage - 1) then
  begin
    DoBackground(FCurrentPage);
    DoAnnotation(FCurrentPage);
    FPaperView.Refresh;
  end;
  UpdateThumbnailPage(PageIndex);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPrintPreview.SetPaperViewOptions(Value: TPaperPreviewOptions);
begin
  FPaperViewOptions.Assign(Value);
end;

procedure TPrintPreview.SetUnits(Value: TUnits);
begin
  if FUnits <> Value then
  begin
    if FPaperType <> pCustom then
      with PaperSizes[FPaperType] do
        FPageExt := ConvertXY(Width, Height, Units, Value)
    else
      ConvertPoints(FPageExt, 1, FUnits, Value);
    if Assigned(FPageCanvas) then
    begin
      FPageCanvas.Pen.Width := ConvertX(FPageCanvas.Pen.Width, FUnits, Value);
      ScaleCanvas(FPageCanvas);
    end;
    FUnits := Value;
  end;
end;

procedure TPrintPreview.SetPaperType(Value: TPaperType);
begin
  if (FPaperType <> Value) and (FState = psReady) then
  begin
    FPaperType := Value;
    FFormName := '';
    if FPaperType <> pCustom then
    begin
      with PaperSizes[FPaperType] do
        FPageExt := ConvertXY(Width, Height, Units, FUnits);
      if FOrientation = poLandscape then
        SwapValues(FPageExt.X, FPageExt.Y);
      CalculateMetafileSize;
      UpdateZoom;
    end;
  end;
end;

procedure TPrintPreview.SetPaperTypeByID(ID: Integer);
var
  Paper: TPaperType;
begin
  for Paper := Low(TPaperType) to High(TPaperType) do
    if PaperSizes[Paper].ID = ID then
    begin
      SetPaperType(Paper);
      Exit;
    end;
  SetPaperType(pCustom);
end;

procedure TPrintPreview.SetPaperSize(Width, Height: Integer);
begin
  if Width > High(SmallInt) then
    Width := High(SmallInt)
  else if Width < 1 then
    Width := 1;
  if Height > High(SmallInt) then
    Height := High(SmallInt)
  else if Height < 1 then
    Height := 1;

  if ((FPageExt.X <> Width) or (FPageExt.Y <> Height)) and (FState = psReady) then
  begin
    FPageExt.X := Width;
    FPageExt.Y := Height;
    if Orientation = poPortrait then
      FPaperType := FindPaperType(FPageExt.X, FPageExt.Y, Units)
    else
      FPaperType := FindPaperType(FPageExt.Y, FPageExt.X, Units);
    FFormName := '';
    CalculateMetafileSize;
    UpdateZoom;
  end;
end;

function TPrintPreview.GetPaperWidth: Integer;
begin
  Result := FPageExt.X;
end;

procedure TPrintPreview.SetPaperWidth(Value: Integer);
begin
  SetPaperSize(Value, FPageExt.Y);
end;

function TPrintPreview.GetPaperHeight: Integer;
begin
  Result := FPageExt.Y;
end;

procedure TPrintPreview.SetPaperHeight(Value: Integer);
begin
  SetPaperSize(FPageExt.X, Value);
end;

function TPrintPreview.GetPageBounds: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.BottomRight := FPageExt;
end;

function TPrintPreview.GetPrinterPageBounds: TRect;
var
  Offset: TPoint;
  PrintArea: TPoint;
  DPI: TPoint;
begin
  Result := PageBounds;
  if PrinterInstalled then
  begin
    DPI.X := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
    DPI.Y := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
    Offset.X := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
    Offset.Y := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
    Offset.X := ConvertUnits(Offset.X, DPI.X, mmPixel, Units);
    Offset.Y := ConvertUnits(Offset.Y, DPI.Y, mmPixel, Units);
    //InflateRect(Result, -Offset.X, -Offset.Y);                                         //Mixy
    PrintArea.X := GetDeviceCaps(Printer.Handle, HORZRES);                               //Mixy
    PrintArea.Y := GetDeviceCaps(Printer.Handle, VERTRES);                               //Mixy
    PrintArea.X := ConvertUnits(PrintArea.X, DPI.X, mmPixel, Units);                     //Mixy
    PrintArea.Y := ConvertUnits(PrintArea.Y, DPI.Y, mmPixel, Units);                     //Mixy
    SetRect(Result, Offset.X, Offset.Y, PrintArea.X + Offset.X, PrintArea.Y + Offset.Y); //Mixy
  end;
end;

function TPrintPreview.IsCustomPaper: Boolean;
begin
  Result := (FPaperType = pCustom);
end;

procedure TPrintPreview.SetOrientation(Value: TPrinterOrientation);
begin
  if (FOrientation <> Value) and (FState = psReady) then
  begin
    FOrientation := Value;
    SwapValues(FPageExt.X, FPageExt.Y);
    CalculateMetafileSize;
    UpdateZoom;
  end;
end;

procedure TPrintPreview.SetZoom(Value: Integer);
var
  OldZoom: Integer;
begin
  if Value < FZoomMin then Value := FZoomMin
  else if Value > FZoomMax then Value := FZoomMax;
  if (FZoom <> Value) or (FZoomState <> zsZoomOther) then
  begin
    OldZoom := FZoom;
    FZoom := Value;
    FZoomState := zsZoomOther;
    UpdateZoom;
    if not FPaperView.OffScreenValid then
    begin
      FZoom := OldZoom;
      UpdateZoom;
    end
    else if Assigned(FOnZoomChange) then
      FOnZoomChange(Self);
  end;
end;

function TPrintPreview.IsZoomStored: Boolean;
begin
  Result := (FZoomState = zsZoomOther) and (FZoom <> 100);
end;

procedure TPrintPreview.SetZoomMin(Value: Integer);
begin
  if (FZoomMin <> Value) and (Value >= 1) and (Value <= FZoomMax) then
  begin
    FZoomMin := Value;
    if (FZoomState = zsZoomOther) and (FZoom < FZoomMin) then
      Zoom := FZoomMin;
  end;
end;

procedure TPrintPreview.SetZoomMax(Value: Integer);
begin
  if (FZoomMax <> Value) and (Value >= FZoomMin) then
  begin
    FZoomMax := Value;
    if (FZoomState = zsZoomOther) and (FZoom > FZoomMax) then
      Zoom := FZoomMax;
  end;
end;

procedure TPrintPreview.SetZoomState(Value: TZoomState);
begin
  if FZoomState <> Value then
  begin
    FZoomState := Value;
    UpdateZoom;
  end;
end;

procedure TPrintPreview.SetCurrentPage(Value: Integer);
begin
  if TotalPages <> 0 then
  begin
    if Value < 1 then Value := 1;
    if Value > TotalPages then Value := TotalPages;
    if FCurrentPage <> Value then
    begin
      FCurrentPage := Value;
      DoBackground(FCurrentPage);
      DoAnnotation(FCurrentPage);
      FPaperView.Refresh;
      UpdateThumbnailViews(False);
      if Assigned(FOnChange) then
        FOnChange(Self);
    end;
  end;
end;

function TPrintPreview.GetUseTempFile: Boolean;
begin
  Result := FPages.UseTempFile;
end;

procedure TPrintPreview.SetUseTempFile(Value: Boolean);
begin
  FPages.UseTempFile := Value;
end;

procedure TPrintPreview.SetGrayscale(Value: TGrayscaleOptions);
begin
  if Grayscale <> Value then
  begin
    FGrayscale := Value;
    FPaperView.Refresh;
    UpdateThumbnailPage(-1);
  end;
end;

function TPrintPreview.GetTotalPages: Integer;
begin
  if FDirectPrinting then
    Result := FDirectPrintPageCount
  else
    Result := FPages.Count;
end;

function TPrintPreview.GetPages(PageNo: Integer): TMetafile;
begin
  if (PageNo >= 1) and (PageNo <= TotalPages) then
    Result := FPages[PageNo-1]
  else
    Result := nil;
end;

function TPrintPreview.GetCanvas: TCanvas;
begin
  if Assigned(FPageCanvas) then
    Result := FPageCanvas
  else
    Result := Printer.Canvas;
end;

function TPrintPreview.GetPrinterInstalled: Boolean;
begin
  Result := (Printer.Printers.Count > 0);
end;

procedure TPrintPreview.ScaleCanvas(ACanvas: TCanvas);
var
  FontSize: Integer;
  LogExt, DevExt: TPoint;
begin
  LogExt := FPageExt;
  DevExt.X := ConvertUnits(LogExt.X,
    GetDeviceCaps(ACanvas.Handle, LOGPIXELSX), FUnits, mmPixel);
  DevExt.Y := ConvertUnits(LogExt.Y,
    GetDeviceCaps(ACanvas.Handle, LOGPIXELSY), FUnits, mmPixel);
  SetMapMode(ACanvas.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(ACanvas.Handle, LogExt.X, LogExt.Y, nil);
  SetViewPortExtEx(ACanvas.Handle, DevExt.X, DevExt.Y, nil);
  SetViewportOrgEx(ACanvas.Handle,
    -GetDeviceCaps(ACanvas.Handle, PHYSICALOFFSETX),
    -GetDeviceCaps(ACanvas.Handle, PHYSICALOFFSETY), nil);
  FontSize := ACanvas.Font.Size;
  ACanvas.Font.PixelsPerInch :=
    MulDiv(GetDeviceCaps(ACanvas.Handle, LOGPIXELSY), LogExt.Y, DevExt.Y);
  ACanvas.Font.Size := FontSize;
end;

procedure TPrintPreview.CalculateMetafileSize;
begin
  FDeviceExt := ConvertXY(FPageExt.X, FPageExt.Y, FUnits, mmPixel);
end;

procedure TPrintPreview.CreateMetafileCanvas(out AMetafile: TMetafile;
  out ACanvas: TCanvas);
begin
  AMetafile := TMetafile.Create;
  try
    AMetafile.Width := FDeviceExt.X;
    AMetafile.Height := FDeviceExt.Y;
    ACanvas := TMetafileCanvas.Create(AMetafile, 0);
    if ACanvas.Handle = 0 then
    begin
      ACanvas.Free;
      ACanvas := nil;
      RaiseOutOfMemory;
    end;
  except
    AMetafile.Free;
    AMetafile := nil;
    raise;
  end;
  ACanvas.Font.Assign(Font);
  ScaleCanvas(ACanvas);
end;

procedure TPrintPreview.CloseMetafileCanvas(var AMetafile: TMetafile;
  var ACanvas: TCanvas);
begin
  ACanvas.Free;
  ACanvas := nil;
  if AMetafile.Handle = 0 then
  begin
    AMetafile.Free;
    AMetafile := nil;
    RaiseOutOfMemory;
  end;
end;

procedure TPrintPreview.CreatePrinterCanvas(out ACanvas: TCanvas);
begin
  ACanvas := TCanvas.Create;
  try
    ACanvas.Handle := Printer.Handle;
    ScaleCanvas(ACanvas);
  except
    ACanvas.Free;
    ACanvas := nil;
    raise;
  end;
end;

procedure TPrintPreview.ClosePrinterCanvas(var ACanvas: TCanvas);
begin
  ACanvas.Handle := 0;
  ACanvas.Free;
  ACanvas := nil;
end;

procedure TPrintPreview.Clear;
begin
  FPages.Clear;
end;

procedure TPrintPreview.BeginDoc;
begin
  if FState = psReady then
  begin
    FPageCanvas := nil;
    FAborted := False;
    if not FDirectPrint then
    begin
      Clear;
      if UsePrinterOptions then
        GetPrinterOptions;
      FState := psCreating;
      FDirectPrinting := False;
      CalculateMetafileSize;
    end
    else
    begin
      FDirectPrinting := True;
      FDirectPrintPageCount := 0;
      if UsePrinterOptions then
        GetPrinterOptions
      else
        SetPrinterOptions;
      FState := psPrinting;
      Printer.Title := PrintJobTitle;
      Printer.BeginDoc;
    end;
    if Assigned(FOnBeginDoc) then
      FOnBeginDoc(Self);
    NewPage;
  end
end;

procedure TPrintPreview.EndDoc;
begin
  if ((FState = psCreating) and not FDirectPrinting) or
     ((FState = psPrinting) and FDirectPrinting) then
  begin
    if Assigned(FOnEndPage) then
      FOnEndPage(Self);
    if not FDirectPrinting then
    begin
      CloseMetafileCanvas(PageMetafile, FPageCanvas);
      try
        FPages.Add(PageMetafile);
      finally
        PageMetafile.Free;
        PageMetafile := nil;
      end;
    end
    else
    begin
      Inc(FDirectPrintPageCount);
      ClosePrinterCanvas(FPageCanvas);
      Printer.EndDoc;
      FDirectPrinting := False;
    end;
    if Assigned(FOnEndDoc) then
      FOnEndDoc(Self);
    FState := psReady;
  end;
end;

procedure TPrintPreview.NewPage;
begin
  if ((FState = psCreating) and not FDirectPrinting) or
     ((FState = psPrinting) and FDirectPrinting) then
  begin
    if Assigned(FPageCanvas) and Assigned(FOnEndPage) then
      FOnEndPage(Self);
    if not FDirectPrinting then
    begin
      if Assigned(FPageCanvas) then
      begin
        CloseMetafileCanvas(PageMetafile, FPageCanvas);
        try
          FPages.Add(PageMetafile);
        finally
          PageMetafile.Free;
          PageMetafile := nil;
        end;
      end;
      CreateMetafileCanvas(PageMetafile, FPageCanvas);
    end
    else
    begin
      if Assigned(FPageCanvas) then
      begin
        Inc(FDirectPrintPageCount);
        Printer.NewPage;
      end
      else
        CreatePrinterCanvas(FPageCanvas);
      FPageCanvas.Font.Assign(Font);
    end;
    if Assigned(FOnNewPage) then
      FOnNewPage(Self);
  end;
end;

procedure TPrintPreview.Abort;
begin
  if not FAborted and (FState <> psReady) then
  begin
    FAborted := True;
    case State of
      psCreating:
      begin
        CloseMetafileCanvas(PageMetafile, FPageCanvas);
        if Assigned(PageMetafile) then
        begin
          PageMetafile.Free;
          PageMetafile := nil;
        end;
        Clear;
      end;
      psPrinting:
      begin
        if Printer.Printing and not Printer.Aborted then
          Printer.Abort;
      end;
    end;
    if Assigned(FOnAbort) then
      FOnAbort(Self);
    FState := psReady;
  end;
end;

function TPrintPreview.BeginEdit(PageNo: Integer): Boolean;
begin
  Result := False;
  if (FState = psReady) and (PageNo > 0) and (PageNo <= TotalPages) then
  begin
    FState := psEditing;
    CalculateMetafileSize;
    CreateMetafileCanvas(PageMetafile, FPageCanvas);
    FPages.LoadedIndex := PageNo - 1;
    try
      FPageCanvas.StretchDraw(PageBounds, FPages.LoadedMetafile);
    finally
      FPages.LoadedIndex := FCurrentPage - 1;
    end;
    FEditingPage := PageNo;
    Result := True;
  end;
end;

procedure TPrintPreview.EndEdit;
begin
  if FState = psEditing then
  begin
    CloseMetafileCanvas(PageMetafile, FPageCanvas);
    try
      FPages.LoadedIndex := FEditingPage - 1;
      try
        FPages.LoadedMetafile.Assign(PageMetafile);
      finally
        FPages.LoadedIndex := FCurrentPage - 1;
      end;
    finally
      PageMetafile.Free;
      PageMetafile := nil;
    end;
    FEditingPage := 0;
    FState := psReady;
  end;
end;

procedure TPrintPreview.LoadFromStream(Stream: TStream);
var
  {$IFDEF ZLIB}
  ZLibID: Word;
  {$ENDIF}
  Data: Integer;
  SavedPos: Integer;
  DecoderStream: TStream;
begin
  {$IFDEF ZLIB}
  SavedPos := Stream.Position;
  if (Stream.Read(ZLibID, SizeOf(ZLibID)) = SizeOf(ZLibID)) and (ZLibID = ZLibSignature) then
  begin
    Stream.Position := SavedPos;
    DecoderStream := TDecompressionStream.Create(Stream);
  end
  else
  begin
    Stream.Position := SavedPos;
    DecoderStream := Stream;
  end;
  {$ELSE}
  DecoderStream := Stream;
  {$ENDIF}
  SavedPos := DecoderStream.Position;
  if (DecoderStream.Read(Data, SizeOf(Data)) = SizeOf(Data)) and (Data = PageInfoSignature) then
  begin
    DecoderStream.Read(Data, SizeOf(Data));
    FOrientation := TPrinterOrientation(Data);
    DecoderStream.Read(Data, SizeOf(Data));
    FPaperType := TPaperType(Data);
    DecoderStream.Read(Data, SizeOf(Data));
    FPageExt.X := ConvertX(Data, mmHiMetric, FUnits);
    DecoderStream.Read(Data, SizeOf(Data));
    FPageExt.Y := ConvertY(Data, mmHiMetric, FUnits);
    CalculateMetafileSize;
  end
  else
    DecoderStream.Position := SavedPos;
  FPages.LoadFromStream(DecoderStream);
  {$IFDEF ZLIB}
  if DecoderStream <> Stream then DecoderStream.Free;
  {$ENDIF}
end;

procedure TPrintPreview.SaveToStream(Stream: TStream);
var
  Data: Integer;
  EncoderStream: TStream;
begin
  {$IFDEF ZLIB}
  EncoderStream := TCompressionStream.Create(clDefault, Stream);
  {$ELSE}
  EncoderStream := Stream;
  {$ENDIF}
  Data := PageInfoSignature;
  EncoderStream.Write(Data, SizeOf(Data));
  Data := Ord(FOrientation);
  EncoderStream.Write(Data, SizeOf(Data));
  Data := Ord(FPaperType);
  EncoderStream.Write(Data, SizeOf(Data));
  Data := ConvertX(FPageExt.X, FUnits, mmHiMetric);
  EncoderStream.Write(Data, SizeOf(Data));
  Data := ConvertY(FPageExt.Y, FUnits, mmHiMetric);
  EncoderStream.Write(Data, SizeOf(Data));
  FPages.SaveToStream(EncoderStream);
  {$IFDEF ZLIB}
  EncoderStream.Free;
  {$ENDIF}
end;

procedure TPrintPreview.LoadFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPrintPreview.SaveToFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TPrintPreview.SaveAsPDF(const FileName: String);

  function PaperTypeToPDFPageSize(PaperType: TPaperType): Integer;
  begin
    case PaperType of
      pCustom     : Result := 00;
      pLetter     : Result := 01;
      pLegal      : Result := 04;
      pExecutive  : Result := 11;
      pA3         : Result := 03;
      pA4         : Result := 02;
      pA5         : Result := 09;
      pB4         : Result := 08;
      pB5         : Result := 05;
      pFolio      : Result := 10;
      pEnvDL      : Result := 15;
      pEnvB4      : Result := 12;
      pEnvB5      : Result := 13;
      pEnvMonarch : Result := 16;
    else
      Result := 0; // Default to custom
    end;
  end;

var
  I: Integer;
  Mem: TMemoryStream;
begin
  if dsPDF.Handle > 0 then
  begin
    dsPDF.BeginDoc(PChar(FileName));
    try
      Mem := TMemoryStream.Create;
      try
        for I := 1 to TotalPages do
        begin
          Mem.Clear;
          Pages[I].SaveToStream(Mem);
          if I > 1 then dsPDF.NewPage;
          dsPDF.SetPage(PaperTypeToPDFPageSize(PaperType), Ord(Orientation),
            ConvertX(PaperWidth, Units, mmHiMetric),
            ConvertY(PaperHeight, Units, mmHiMetric));
          dsPDF.PrintPageMemory(Mem.Memory, Mem.Size);
        end;
      finally
        Mem.Free;
      end;
    finally
      dsPDF.EndDoc;
    end;
  end
  else
    raise EMissingPDFLibrary.Create('Cannot locate ' + dsPDF_lib);
end;

function TPrintPreview.CanSaveAsPDF: Boolean;
begin
  Result := (dsPDF.Handle > 0);
end;

procedure TPrintPreview.Print;
begin
  PrintPages(1, TotalPages);
end;

procedure TPrintPreview.PrintPages(FirstPage, LastPage: Integer);

  procedure ResetPrinterDC;
  var
    DeviceMode: THandle;
    DevMode: PDeviceMode;
    Device, Driver, Port: array[0..MAX_PATH] of Char;
  begin
    Printer.GetPrinter(Device, Driver, Port, DeviceMode);
    DevMode := PDevMode(GlobalLock(DeviceMode));
    try
      ResetDC(Printer.Canvas.Handle, DevMode^);
    finally
      GlobalUnlock(DeviceMode);
    end;
  end;

  procedure InitializePrinter;
  begin
    if Assigned(FOnBeforePrint) then
      FOnBeforePrint(Self);
    if not UsePrinterOptions then
      SetPrinterOptions;
    Printer.Title := PrintJobTitle;
    Printer.BeginDoc;
    if not UsePrinterOptions then
      ResetPrinterDC;
  end;

  procedure FinalizePrinter(Succeed :Boolean);
  begin
    FAborted := FAborted or not Succeed;
    if not Succeed and Printer.Printing then
      Printer.Abort;
    if Printer.Printing then
      Printer.EndDoc;
    Printer.Title := EmptyStr;
    if Assigned(FOnAfterPrint) then
      FOnAfterPrint(Self);
  end;

  function GetPhysicalPageBounds: TRect;
  begin
    Result.Left := 0;
    Result.Top := 0;
    if UsePrinterOptions then
    begin
      Result.Right := GetDeviceCaps(Printer.Handle, PHYSICALWIDTH);
      Result.Bottom := GetDeviceCaps(Printer.Handle, PHYSICALHEIGHT);
    end
    else
    begin
      Result.Right := ConvertUnits(FPageExt.X,
        GetDeviceCaps(Printer.Handle, LOGPIXELSX), FUnits, mmPixel);
      Result.Bottom := ConvertUnits(FPageExt.Y,
        GetDeviceCaps(Printer.Handle, LOGPIXELSY), FUnits, mmPixel);
    end;
    OffsetRect(Result,
       -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX),
       -GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY));
  end;

var
  CurPage: Integer;
  PageRect: TRect;
  Done: Boolean;
begin
  if (FState = psReady) and (Printer.Printers.Count > 0) and
     (TotalPages > 0) and (FirstPage <= LastPage) then
  begin
    try
      Done := False;
      FAborted := False;
      CurPage := FirstPage;
      FState := psPrinting;
      try
        InitializePrinter;
        PageRect := GetPhysicalPageBounds;
        while not FAborted and (CurPage <= LastPage) do
        begin
          DoProgress(CurPage, CurPage - FirstPage, LastPage - FirstPage + 1);
          if not FAborted then
          begin
            if gsPrint in FGrayscale then
              StretchDrawGrayscale(Printer.Canvas, PageRect, FPages[CurPage-1])
            else
              Printer.Canvas.StretchDraw(PageRect, FPages[CurPage-1]);
          end;
          DoProgress(CurPage, CurPage - FirstPage + 1, LastPage - FirstPage + 1);
          if not FAborted then
          begin
            if CurPage < LastPage then
              Printer.NewPage
            else
              Done := True;
          end;
          Inc(CurPage);
        end;
      finally
        FinalizePrinter(Done);
        FState := psReady;
      end;
    except
      on EAbort do
        // Do nothing
      else
        raise;
    end;
  end;
end;

procedure TPrintPreview.DoProgress(Current, Done, Total: Integer);
var
  Cencelled: Boolean;
begin
  Cencelled := FAborted;
  if not FAborted and Assigned(FOnPrintProgress) then
    FOnPrintProgress(Self, Current, MulDiv(100, Done, Total), Cencelled);
  if not FAborted and Cencelled then
    Abort;
end;

procedure TPrintPreview.RegisterThumbnailView(ThumbnailView: TThumbnailPreview);
begin
  if ThumbnailView <> nil then
  begin
    if FThumbnailViews = nil then
      FThumbnailViews := TList.Create;
    if FThumbnailViews.IndexOf(ThumbnailView) < 0 then
      FThumbnailViews.Add(ThumbnailView);
  end;
end;

procedure TPrintPreview.UnregisterThumbnailView(ThumbnailView: TThumbnailPreview);
begin
  if FThumbnailViews <> nil then
  begin
    FThumbnailViews.Remove(ThumbnailView);
    if FThumbnailViews.Count = 0 then
    begin
      FThumbnailViews.Free;
      FThumbnailViews := nil;
    end;
  end;
end;

procedure TPrintPreview.UpdateThumbnailViews(Rebuild: Boolean);
var
  I: Integer;
begin
  if FThumbnailViews <> nil then
    for I := 0 to FThumbnailViews.Count - 1 do
      TThumbnailPreview(FThumbnailViews[I]).UpdateThumbnails(Rebuild);
end;

procedure TPrintPreview.UpdateThumbnailPage(PageIndex: Integer);
var
  I: Integer;
begin
  if FThumbnailViews <> nil then
    for I := 0 to FThumbnailViews.Count - 1 do
      TThumbnailPreview(FThumbnailViews[I]).UpdatePage(PageIndex);
end;

procedure TPrintPreview.SetAnnotation(Value: Boolean);
begin
  if FAnnotation <> Value then
  begin
    FAnnotation := Value;
    DoAnnotation(FCurrentPage);
    FPaperView.Refresh;
  end;
end;

procedure TPrintPreview.UpdateAnnotation;
begin
  if FAnnotation then
  begin
    DoAnnotation(FCurrentPage);
    FPaperView.Refresh;
  end;
end;

procedure TPrintPreview.DoAnnotation(PageNo: Integer);
var
  AnnotationCanvas: TCanvas;
begin
  if Assigned(AnnotationMetafile) then
  begin
    AnnotationMetafile.Free;
    AnnotationMetafile := nil;
  end;
  if FAnnotation and (PageNo > 0) and Assigned(FOnAnnotation) then
  begin
    CreateMetafileCanvas(AnnotationMetafile, AnnotationCanvas);
    try
      FOnAnnotation(Self, PageNo, AnnotationCanvas);
    finally
      CloseMetafileCanvas(AnnotationMetafile, AnnotationCanvas);
    end;
  end
end;

procedure TPrintPreview.SetBackground(Value: Boolean);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    DoBackground(FCurrentPage);
    FPaperView.Refresh;
  end;
end;

procedure TPrintPreview.UpdateBackground;
begin
  if FBackground then
  begin
    DoBackground(FCurrentPage);
    FPaperView.Refresh;
  end;
end;

procedure TPrintPreview.DoBackground(PageNo: Integer);
var
  BackgroundCanvas: TCanvas;
begin
  if Assigned(BackgroundMetafile) then
  begin
    BackgroundMetafile.Free;
    BackgroundMetafile := nil;
  end;
  if FBackground and (PageNo > 0) and Assigned(FOnBackground) then
  begin
    CreateMetafileCanvas(BackgroundMetafile, BackgroundCanvas);
    try
      FOnBackground(Self, PageNo, BackgroundCanvas);
    finally
      CloseMetafileCanvas(BackgroundMetafile, BackgroundCanvas);
    end;
  end
end;

{ TThumbnail }

constructor TThumbnail.Create(AOwner: TThumbnailPreview; APageNo: Integer);
begin
  PageNo := APageNo;
  PageView := TPaperPreview.Create(AOwner);
  PageView.Tag := PageNo;
  PageView.TabStop := False;
  PageView.OnPaint := AOwner.ThumbnailPaint;
  PageView.OnClick := AOwner.ThumbnailClick;
  AOwner.PaperView.AssignTo(PageView);
  PageLabel := TLabel.Create(AOwner);
  PageLabel.Tag := PageNo;
  PageLabel.AutoSize := False;
  PageLabel.Alignment := taCenter;
  PageLabel.ShowAccelChar := False;
  PageLabel.Transparent := True;
  PageLabel.Caption := IntToStr(PageNo);
  PageLabel.FocusControl := PageView;
  PageLabel.OnClick := AOwner.ThumbnailClick;
end;

destructor TThumbnail.Destroy;
begin
  PageLabel.Free;
  PageView.Free;
  inherited Destroy;
end;

function TThumbnail.GetBoundRect: TRect;
begin
  UnionRect(Result, PageLabel.BoundsRect, PageView.BoundsRect);
  InflateRect(Result, 1, 1);
end;

function TThumbnail.HasAsMember(Component: TComponent): Boolean;
begin
  Result := (Component = PageView) or (Component = PageLabel);
end;

{ TThumbnailPreview }

constructor TThumbnailPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  Align := alLeft;
  TabStop := True;
  FZoom := 10;
  FMargin := 6;
  FMarkerColor := clBlue;
  FOrientation  := sbVertical;
  FThumbnailClass := TThumbnail;
  FPaperViewOptions := TPaperPreviewOptions.Create;
  FPaperViewOptions.OnChange := PaperViewOptionsChanged;
  FThumbnails := TList.Create;
end;

destructor TThumbnailPreview.Destroy;
begin
  PrintPreview := nil;
  FPaperViewOptions.Free;
  FThumbnails.Free;
  inherited Destroy;
end;

procedure TThumbnailPreview.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = PrintPreview) then
    PrintPreview := nil;
end;

procedure TThumbnailPreview.Resize;
begin
  inherited Resize;
  UpdateThumbnails(True);
end;

procedure TThumbnailPreview.ThumbnailClick(Sender: TObject);
begin
  PrintPreview.CurrentPage := TPaperPreview(Sender).Tag;
  if TabStop then SetFocus;
end;

procedure TThumbnailPreview.ThumbnailPaint(Sender: TObject;
  Canvas: TCanvas; const PageRect: TRect);
var
  PageNo: Integer;
begin
  PageNo := TPaperPreview(Sender).Tag;
  if gsPreview in PrintPreview.Grayscale then
    StretchDrawGrayscale(Canvas, PageRect, PrintPreview.Pages[PageNo])
  else
    Canvas.StretchDraw(PageRect, PrintPreview.Pages[PageNo]);
  if (ActiveThumb <> nil) and (ActiveThumb.PageNo = PageNo) then
  begin
   Canvas.Brush.Style := bsClear;
   Canvas.Pen.Mode := pmCopy;
   Canvas.Pen.Width := 1;
   Canvas.Pen.Color := MarkerColor;
   with PageRect do Canvas.Rectangle(Left, Top, Right, Bottom);
  end;
end;

procedure TThumbnailPreview.CalculateElementBounds(out ViewPos, ViewSize,
  LabelPos, LabelSize, ThumbSize: TPoint);
var
  Bitmap: TBitmap;
  TextMetric: TTextMetric;
begin
  ViewSize.X := MulDiv(PrintPreview.PagePixels.X, Zoom, 100) +
                2 * PaperView.BorderWidth + PaperView.ShadowWidth;
  ViewSize.Y := MulDiv(PrintPreview.PagePixels.Y, Zoom, 100) +
                2 * PaperView.BorderWidth + PaperView.ShadowWidth;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(Font);
    GetTextMetrics(Bitmap.Canvas.Handle, TextMetric);
  finally
    Bitmap.Free;
  end;
  LabelSize.X := TextMetric.tmMaxCharWidth * Length(IntToStr(PrintPreview.TotalPages));
  LabelSize.Y := TextMetric.tmHeight + TextMetric.tmExternalLeading;
  if ViewSize.X >= LabelSize.X then
  begin
     ViewPos.X := 0;
     LabelPos.X := (ViewSize.X - LabelSize.X) div 2;
     ThumbSize.X := ViewSize.X;
  end
  else
  begin
     LabelPos.X := 0;
     ViewPos.X := (LabelSize.X - ViewSize.X) div 2;
     ThumbSize.X := LabelSize.X;
  end;
  ViewPos.Y := 0;
  LabelPos.Y := ViewPos.Y + ViewSize.Y + 2;
  ThumbSize.Y := LabelPos.Y + LabelSize.Y;
end;

procedure TThumbnailPreview.UpdateThumbnails(Rebuild: Boolean);
var
  I: Integer;
  ThumbPos: TPoint;
  ThumbSize: TPoint;
  ViewPos: TPoint;
  ViewSize: TPoint;
  LabelPos: TPoint;
  LabelSize: TPoint;
  Thumb: TThumbnail;
  HorzSpace, VertSpace: Integer;
begin
  if not Updating then
  begin
    Updating := True;
    try
      if (PrintPreview <> nil) and (PrintPreview.TotalPages > 0) then
      begin
        if Rebuild then
        begin
          HorzScrollBar.Visible := (Orientation = sbHorizontal);
          VertScrollBar.Visible := (Orientation = sbVertical);
          HorzSpace := ClientWidth;
          VertSpace := ClientHeight;
          ThumbPos.X := Margin - HorzScrollBar.Position;
          ThumbPos.Y := Margin - VertScrollBar.Position;
          CalculateElementBounds(ViewPos, ViewSize, LabelPos, LabelSize, ThumbSize);
          for I := 0 to PrintPreview.TotalPages - 1 do
          begin
            if I = FThumbnails.Count then
              FThumbnails.Add(FThumbnailClass.Create(Self, I + 1));
            Thumb := TThumbnail(FThumbnails[I]);
            Thumb.PageView.SetBounds(ThumbPos.X + ViewPos.X, ThumbPos.Y + ViewPos.Y, ViewSize.X, ViewSize.Y);
            Thumb.PageView.Parent := Self;
            Thumb.PageLabel.SetBounds(ThumbPos.X + LabelPos.X, ThumbPos.Y + LabelPos.Y, LabelSize.X, LabelSize.Y);
            Thumb.PageLabel.Parent := Self;
            if Orientation = sbVertical then
            begin
              Inc(ThumbPos.X, ThumbSize.X + Margin);
              if ThumbPos.X + ThumbSize.X > HorzSpace then
              begin
                ThumbPos.X := Margin;
                Inc(ThumbPos.Y, ThumbSize.Y + Margin);
              end;
            end
            else
            begin
              Inc(ThumbPos.Y, ThumbSize.Y + Margin);
              if ThumbPos.Y + ThumbSize.Y > VertSpace then
              begin
                ThumbPos.Y := Margin;
                Inc(ThumbPos.X, ThumbSize.X + Margin);
              end;
            end;
            if Orientation = sbVertical then
            begin
              FColCount := 1;
              if ThumbSize.X > Margin then
              begin
                FColCount := (HorzSpace - Margin) div (ThumbSize.X + Margin);
                if FColCount = 0 then FColCount := 1;
              end;
              FRowCount := PrintPreview.TotalPages div FColCount;
            end
            else
            begin
              FRowCount := 1;
              if ThumbSize.Y > Margin then
              begin
                FRowCount := (VertSpace - Margin) div (ThumbSize.Y - Margin);
                if FRowCount = 0 then FRowCount := 1;
              end;
              FColCount := PrintPreview.TotalPages div FRowCount;
            end;
          end;
          for I := FThumbnails.Count - 1 downto PrintPreview.TotalPages do
          begin
            Thumb := TThumbnail(FThumbnails[I]);
            FThumbnails.Delete(I);
            if Thumb = ActiveThumb then
              ActiveThumb := nil;
            Thumb.Free;
          end;
        end;
        Thumb := nil;
        for I := FThumbnails.Count - 1 downto 0 do
          if TThumbnail(FThumbnails[I]).PageNo = PrintPreview.CurrentPage then
          begin
            Thumb := TThumbnail(FThumbnails[I]);
            Break;
          end;
        if Thumb <> ActiveThumb then
        begin
          if ActiveThumb <> nil then
          begin
            ActiveThumb.PageView.Invalidate;
            ActiveThumb := nil;
          end;
          ActiveThumb := Thumb;
          if ActiveThumb <> nil then
          begin
            MakeVisible(ActiveThumb.GetBoundRect);
            ActiveThumb.PageView.Invalidate;
          end;
          Update;
          if Assigned(FOnChange) then
            FOnChange(Self);
        end;
      end
      else if FThumbnails.Count > 0 then
      begin
        FRowCount := 0;
        FColCount := 0;
        ActiveThumb := nil;
        for I := FThumbnails.Count - 1 downto 0 do
        begin
          TThumbnail(FThumbnails[I]).Free;
          FThumbnails.Delete(I);
        end;
      end;
    finally
      Updating := False;
    end;
  end;
end;

procedure TThumbnailPreview.UpdatePage(Index: Integer);
var
  I: Integer;
begin
   if (Index >= 0) and (Index < FThumbnails.Count) then
     TThumbnail(FThumbnails[Index]).PageView.Refresh
   else if Index = -1 then
   begin
     for I := FThumbnails.Count - 1 downto 0 do
       TThumbnail(FThumbnails[I]).PageView.Invalidate;
     Update;
   end;
end;

procedure TThumbnailPreview.SetPaperViewOptions(Value: TPaperPreviewOptions);
begin
  FPaperViewOptions.Assign(Value);
end;

procedure TThumbnailPreview.SetPrintPreview(Value: TPrintPreview);
begin
  if PrintPreview <> Value then
  begin
    if PrintPreview <> nil then
      PrintPreview.UnregisterThumbnailView(Self);
    FPrintPreview := Value;
    if PrintPreview <> nil then
    begin
      PrintPreview.FreeNotification(Self);
      PrintPreview.RegisterThumbnailView(Self);
    end;
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.SetZoom(Value: Integer);
begin
  if Zoom <> Value then
  begin
    FZoom := Value;
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.SetMargin(Value: Byte);
begin
  if Margin <> Value then
  begin
    FMargin := Value;
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.SetMarkerColor(Value: TColor);
begin
  if MarkerColor <> Value then
  begin
    FMarkerColor := Value;
    if Assigned(ActiveThumb) then
      ActiveThumb.PageView.Invalidate;
  end;
end;

procedure TThumbnailPreview.SetOrientation(Value: TScrollBarKind);
begin
  if Orientation <> Value then
  begin
    FOrientation := Value;
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.SetThumbnailClass(Value: TThumbnailClass);
begin
  if FThumbnailClass <> Value then
  begin
    FThumbnailClass := Value;
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.PaperViewOptionsChanged(Sender: TObject);
var
  I: Integer;
begin
  if PrintPreview <> nil then
  begin
    for I := 0 to FThumbnails.Count - 1 do
      FPaperViewOptions.AssignTo(TThumbnail(FThumbnails[I]).PageView);
    UpdateThumbnails(True);
  end;
end;

procedure TThumbnailPreview.CNKeyDown(var Message: TWMKey);
var
  Key: Word;
  Shift: TShiftState;
begin
  if PrintPreview <> nil then
  begin
    with Message do
    begin
      Key := CharCode;
      Shift := KeyDataToShiftState(KeyData);
    end;
    if (Key = VK_LEFT) and (Shift = []) then
      if Orientation = sbHorizontal then
        PrintPreview.CurrentPage := PrintPreview.CurrentPage - RowCount
      else
        PrintPreview.CurrentPage := PrintPreview.CurrentPage - 1
    else if (Key = VK_RIGHT) and (Shift = []) then
      if Orientation = sbHorizontal then
        PrintPreview.CurrentPage := PrintPreview.CurrentPage + RowCount
      else
        PrintPreview.CurrentPage := PrintPreview.CurrentPage + 1
    else if (Key = VK_UP) and (Shift = []) then
      if Orientation = sbVertical then
        PrintPreview.CurrentPage := PrintPreview.CurrentPage - ColCount
      else
        PrintPreview.CurrentPage := PrintPreview.CurrentPage - 1
    else if (Key = VK_DOWN) and (Shift = []) then
      if Orientation = sbVertical then
        PrintPreview.CurrentPage := PrintPreview.CurrentPage + ColCount
      else
        PrintPreview.CurrentPage := PrintPreview.CurrentPage + 1
    else if (Key = VK_NEXT) and (Shift = []) then
      PrintPreview.CurrentPage := PrintPreview.CurrentPage + 1
    else if (Key = VK_NEXT) and (Shift = [ssCtrl]) then
      PrintPreview.CurrentPage := PrintPreview.TotalPages
    else if (Key = VK_PRIOR) and (Shift = []) then
      PrintPreview.CurrentPage := PrintPreview.CurrentPage - 1
    else if (Key = VK_PRIOR) and (Shift = [ssCtrl]) then
      PrintPreview.CurrentPage := 1
    else
      inherited;
  end
  else
    inherited;
end;

procedure TThumbnailPreview.WMMouseWheel(var Message: TMessage);
var
  IsNeg: Boolean;
  Rect: TRect;
  Pt: TPoint;
begin
  GetWindowRect(WindowHandle, Rect);
  Pt.X := Message.LParamLo;
  Pt.Y := Message.LParamHi;
  if PtInRect(Rect, Pt) then
  begin
    Message.Result := 1;
    Inc(WheelAccumulator, SmallInt(Message.WParamLo));
    while Abs(WheelAccumulator) >= WHEEL_DELTA do
    begin
      IsNeg := WheelAccumulator < 0;
      WheelAccumulator := Abs(WheelAccumulator) - WHEEL_DELTA;
      if IsNeg then
      begin
        WheelAccumulator := -WheelAccumulator;
        if Orientation = sbVertical then
          with VertScrollBar do Position := Position + Increment
        else
          with HorzScrollBar do Position := Position + Increment;
      end
      else
      begin
        if Orientation = sbVertical then
          with VertScrollBar do Position := Position - Increment
        else
          with HorzScrollBar do Position := Position - Increment;
      end;
    end;
  end;
end;

procedure TThumbnailPreview.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateThumbnails(True);
end;

procedure TThumbnailPreview.MakeVisible(const Bounds: TRect);
var
  Rect: TRect;
begin
  Rect := Bounds;
  Dec(Rect.Left, HorzScrollBar.Margin);
  Inc(Rect.Right, HorzScrollBar.Margin);
  Dec(Rect.Top, VertScrollBar.Margin);
  Inc(Rect.Bottom, VertScrollBar.Margin);
  if Rect.Left < 0 then
    with HorzScrollBar do Position := Position + Rect.Left
  else if Rect.Right > ClientWidth then
  begin
    if Rect.Right - Rect.Left > ClientWidth then
      Rect.Right := Rect.Left + ClientWidth;
    with HorzScrollBar do Position := Position + Rect.Right - ClientWidth;
  end;
  if Rect.Top < 0 then
    with VertScrollBar do Position := Position + Rect.Top
  else if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    with VertScrollBar do Position := Position + Rect.Bottom - ClientHeight;
  end;
end;

procedure LoadPDF;
begin
  dsPDF.Handle := LoadLibrary(dsPDF_lib);
  if dsPDF.Handle > 0 then
  begin
    @dsPDF.BeginDoc := GetProcAddress(dsPDF.Handle, 'BeginDoc');
    @dsPDF.EndDoc := GetProcAddress(dsPDF.Handle, 'EndDoc');
    @dsPDF.NewPage := GetProcAddress(dsPDF.Handle, 'NewPage');
    @dsPDF.PrintPageMemory := GetProcAddress(dsPDF.Handle, 'PrintPageM');
    @dsPDF.PrintPageFile := GetProcAddress(dsPDF.Handle, 'PrintPageF');
    @dsPDF.SetPage := GetProcAddress(dsPDF.Handle, 'SetPage');
  end;
end;

procedure UnloadPDF;
begin
  if dsPDF.Handle > 0 then
  begin
    FreeLibrary(dsPDF.Handle);
    FillChar(dsPDF, SizeOf(dsPDF), 0);
  end;
end;

initialization
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'CURSOR_HAND');
  Screen.Cursors[crGrab] := LoadCursor(hInstance, 'CURSOR_GRAB');
  LoadPDF;
finalization
  UnloadPDF;
end.
