unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Activex, ComObj ,
  Tracetool;
type
  TForm1 = class(TForm)
    SafeArrayCreate1: TButton;
    butArrayCreate: TButton;
    butSA_Integer: TButton;
    butSA_String: TButton;
    procedure SafeArrayCreate1Click(Sender: TObject);
    procedure butArrayCreateClick(Sender: TObject);
    procedure butSA_IntegerClick(Sender: TObject);
    procedure butSA_StringClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.butArrayCreateClick(Sender: TObject);
var
   master,arr2,arr3, arr4 : OleVariant ;
begin

  // create the master array
  master := varArrayCreate ([0,1],varVariant) ;

  // create the second array
  arr2 := varArrayCreate ([0,1],varVariant) ;
  arr2[0] := 'arr2' ;

  // make a "copy" of arr2 to master
  master [0] := arr2 ;

  arr3 := master [0] ;                       // get a "copy" of first element
  TTrace.Debug.Send (VarToStr(arr3[0])) ;    // print "arr2"
  arr3[0] := 'new val' ;                     // set new value

  arr4 := master [0] ;                       // get a new "copy"
  TTrace.Debug.Send (VarToStr(arr4[0])) ;    // print "arr2" because the master is not updated


  TTrace.Debug.Send (VarToStr(arr2[0])) ;    // still print "arr2"

  master [0] := arr3 ;                       // make a "copy" of arr3 to master

  TTrace.Debug.Send (VarToStr(arr2[0])) ;    // still print "arr2"
  TTrace.Debug.Send (VarToStr(arr4[0])) ;    // print "arr2" because this "copy" is not changed

end;


procedure TForm1.SafeArrayCreate1Click(Sender: TObject);
var
   //str : string ;
   result : OleVariant ;

   psa : psafearray ;          // PSafeArray = ^TSafeArray.   TSafeArray = tagSAFEARRAY = record
   saBound : array [0..1] of TSafeArrayBound;  // record
   c,d : integer ;
   index : array [0..1] of LongInt ;
   v : variant ;
begin

   saBound[0].lLbound := 0 ;
   saBound[0].cElements := 3;

   saBound[1].lLbound := 0 ;
   saBound[1].cElements := 15;

   psa := SafeArrayCreate (VT_VARIANT, 2 {dimension} , saBound) ;

   // put some kind of variant on the first column
   index [0] := 0 ;
   index [1] :=  0 ;                                SafeArrayPutElement(psa, index, v) ;
   index [1] :=  1 ;   v := null ;                  SafeArrayPutElement(psa, index, v) ;
   index [1] :=  2 ;   v := 123 ;                   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  3 ;   v := now ;                   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  4 ;   v := true ;                  SafeArrayPutElement(psa, index, v) ;
   index [1] :=  5 ;   v := 'str' ;                 SafeArrayPutElement(psa, index, v) ;
   index [1] :=  6 ;   v := 100 / 3 ;               SafeArrayPutElement(psa, index, v) ;
   index [1] :=  7 ;   v := 123 ;                   SafeArrayPutElement(psa, index, v) ;
   index [1] :=  8 ;   tagVARIANT(v).vt := VT_I2 ;  SafeArrayPutElement(psa, index, v) ;
   index [1] :=  9 ;   tagVARIANT(v).vt := VT_I4 ;  SafeArrayPutElement(psa, index, v) ;
   index [1] := 10 ;   tagVARIANT(v).vt := VT_I1  ; SafeArrayPutElement(psa, index, v) ;
   index [1] := 11 ;   tagVARIANT(v).vt := VT_UI1 ; SafeArrayPutElement(psa, index, v) ;

   index [1] := 12 ;
   v := varArrayCreate ([0,1],varVariant) ;
   v[0] := '789' ;
   OleCheck(SafeArrayPutElement(psa, index, v)) ;

   // fill the other cells with integers
   for c := 1 to 2 do begin
      for d := 0 to 14 do begin
         index [0] := c ;   // 0..2
         index [1] := d ;   // 0..14
         v := c * 100 + d ;
         OleCheck(SafeArrayPutElement(psa, index, v)) ;
      end ;
   end ;

   // convert a safearray to a variant (array of variant)
   tagVARIANT(result).vt := VT_VARIANT or VT_ARRAY ;
   tagVARIANT(result).parray := psa ;

   TTrace.debug.SendVariant ('array VT_VARIANT' , result) ;

   // Read array using variant or safearray
   // -------------------------------------
   //   // get integer
   //
   //   // (using variant)
   //   v := TranslationVAR[0,2] ;
   //   TTrace.Debug.Send (VarToStr(v));
   //
   //   // (using safeArray)
   //   index [0] := 0 ;
   //   index [1] := 2 ;
   //   OleCheck(SafeArrayGetElement(TranslationARR, index, v));
   //   TTrace.Debug.Send (VarToStr(v));
   //
   //   // get sub array and print elem[0]
   //
   //   // (using variant)
   //   v := TranslationVAR[0,12] ;
   //   TTrace.Debug.Send (VarToStr(v[0]));  // get first elem
   //
   //   // (using safeArray)
   //   index [0] := 0 ;
   //   index [1] := 12 ;
   //   OleCheck(SafeArrayGetElement(TranslationARR, index, v));
   //   TTrace.Debug.Send (VarToStr(v[0]));  // get first elem
   //
   //
end;

// read / write integer to safe array (1 dimension)

procedure TForm1.butSA_IntegerClick(Sender: TObject);
var
   TranslationARR : psafearray ;
   saBound :  TSafeArrayBound;
   index : array [0..0] of LongInt ;   // 1 dimension
   resultInt : LongInt ;
   TranslationVAR : Variant ;
   //resultVAR : Variant ;
begin
   saBound.lLbound := 0 ;
   saBound.cElements := 5;

   // create the array
   TranslationARR := activeX.SafeArrayCreate (varInteger , 1 , saBound) ;
   if TranslationARR = nil then begin
      TTrace.Debug.Send ('arr is nil') ;
      exit ;
   end ;

   // assign a value to the first element of TranslationARR
   resultInt := 123 ;
   index [0] := 0 ;
   OleCheck(SafeArrayPutElement(TranslationARR, index, resultInt)) ;

   // convert a safearray to a variant (array of integer)
   tagVARIANT(TranslationVAR).vt := varInteger or varArray;
   tagVARIANT(TranslationVAR).parray := TranslationARR;

   // display the variant
   TTrace.Debug.SendVariant ('Array varInteger' , TranslationVAR) ;

   // Read safearray using variant or safearray
   // -----------------------------------------
   //   // get first (using variant)
   //   resultVAR := TranslationVAR[0] ;
   //   TTrace.Debug.Send (VarToStr(resultVAR));
   //
   //   // get first (using SafeArray)
   //   resultInt := 0 ;  // reset before use
   //   index [0] := 0 ;
   //   OleCheck(SafeArrayGetElement(TranslationARR, index, resultInt));
   //   TTrace.Debug.Send (intToStr(resultInt));

end;


// read / write string to safe array (1 dimension)
procedure TForm1.butSA_StringClick(Sender: TObject);
var
   //pc : pchar ;
   //str : string ;
   // resultVAR : Variant ;
   TranslationARR : psafearray ;
   saBound :  TSafeArrayBound;
   index : array [0..0] of LongInt ;   // 1 dimension
   TranslationVAR : Variant ;
   ResultBStr : TBStr; // same as PWideChar ;
begin
   saBound.lLbound := 0 ;
   saBound.cElements := 5;

   // create the array
   TranslationARR := SafeArrayCreate (VT_BSTR , 1 , saBound) ;
   if TranslationARR = nil then begin
      TTrace.Debug.Send ('arr is nil') ;
      exit ;
   end ;

   // assign a value to the first element of TranslationARR

   index [0] := 0 ;
   ResultBStr := '456' ;
   OleCheck(SafeArrayPutElement(TranslationARR, index, ResultBStr^)) ;

   // convert a safearray to a variant (array of string)
   tagVARIANT(TranslationVAR).vt := varOleStr or varArray;
   tagVARIANT(TranslationVAR).parray := TranslationARR;

   // display the variant
   TTrace.Debug.SendVariant ('Array VT_BSTR' , TranslationVAR) ;

   // Read safearray using variant or safearray
   // -----------------------------------------
   //   // get first (using variant)
   //   resultVAR := TranslationVAR[0] ;
   //   // VarToStr give bad string in case of bstr. We need to use a new copy
   //   str := VarToStr(resultVAR) ;
   //   pc := Pchar (str) ;
   //   str := pc ;
   //   TTrace.Debug.Send (str);
   //
   //   // get first (using SafeArray)
   //   index [0] := 0 ;
   //   ResultBStr := 'reset' ;
   //   OleCheck(SafeArrayGetElement(TranslationARR, index, ResultBStr));
   //   TTrace.Debug.Send (ResultBStr);
end;

end.
