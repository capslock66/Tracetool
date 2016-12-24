{*****************************************************************************
  Name           : PSCHintInfo
  Author         : POL OFFICE COMMON
  Description    :
  History        :

  Date         By                   Description
  ----         --                   -----------
  28-06-2006   POL OFFICE COMMON    Initial creation of the Unit.
 *****************************************************************************}
{$I PSCSystem.INC}

unit PSCHintInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList;

type
  TPSCHintInfo = class(TPersistent)
  private
    FOwner : TPersistent;
    FImageIndex: TImageIndex;
    FHintTranslationID: integer;
    FGlyph: TBitmap;
    FShowHelp: boolean;
    FCaption: TCaption;
    FHint : TStrings;
    FCaptionTranslationID: integer;
    procedure SetGlyph(const Value: TBitmap);
    procedure SetHint(const Value: TStrings);
  protected
    function GetOwner : TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner : TPersistent); virtual;
    destructor Destroy; override;
  published
    property Caption : TCaption read FCaption write FCaption;
    property ImageIndex : TImageIndex read FImageIndex write FImageIndex default -1;
    property HintTranslationID : integer read FHintTranslationID write FHintTranslationID default 0;
    property CaptionTranslationID : integer read FCaptionTranslationID write FCaptionTranslationID default 0;
    property Glyph : TBitmap read FGlyph write SetGlyph;
    property ShowHelp : boolean read FShowHelp write FShowHelp default false;
    property Hint : TStrings read FHint write SetHint;
  end;


implementation


{ TPSCHintInfo }

procedure TPSCHintInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TPSCHintInfo then
   begin
     TPSCHintInfo(Dest).Caption := Self.Caption;
     TPSCHintInfo(Dest).ImageIndex := Self.ImageIndex;
     TPSCHintInfo(Dest).Glyph.Assign(Self.Glyph);
     TPSCHintInfo(Dest).ShowHelp := Self.ShowHelp;
     TPSCHintInfo(Dest).HintTranslationID := Self.HintTranslationID;
     TPSCHintInfo(Dest).CaptionTranslationID := Self.CaptionTranslationID;
     TPSCHintInfo(Dest).Hint.Assign(Self.Hint);
   end
     else
       inherited AssignTo(Dest);
end;

constructor TPSCHintInfo.Create(AOwner : TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FGlyph := TBitmap.Create;
  FImageIndex := -1;
  FHintTranslationID := 0;
  FCaptionTranslationID := 0;
  FShowHelp := false;
  FHint := TStringList.Create;
end;

destructor TPSCHintInfo.Destroy;
begin
  FGlyph.Free;
  FHint.Free;
  inherited;
end;

function TPSCHintInfo.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TPSCHintInfo.SetGlyph(const Value: TBitmap);
begin
  FGlyph.Assign(Value);
end;

procedure TPSCHintInfo.SetHint(const Value: TStrings);
begin
  FHint.Assign(Value);
end;

end.