
{***************************************************************************************}
{                                                                                       }
{                                   XML Data Binding                                    }
{                                                                                       }
{         Generated on: 19/11/2008 20:16:04                                             }
{       Generated from: \\.host\Shared Folders\tracetool\Viewer 2009\Application6.xsd   }
{   Settings stored in: \\.host\Shared Folders\tracetool\Viewer 2009\Application6.xdb   }
{                                                                                       }
{***************************************************************************************}

unit Application6;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLNodeType = interface;
  IXMLNodeTypeList = interface;
  IXMLData = interface;
  IXMLColumn = interface;
  IXMLColumnList = interface;
  IXMLMemberType = interface;
  IXMLMemberTypeList = interface;
  IXMLFontDetail = interface;
  IXMLFontDetailList = interface;

{ IXMLNodeType }

  IXMLNodeType = interface(IXMLNode)
    ['{6E6AC854-CA43-4C73-8B12-D062DF431514}']
    { Property Accessors }
    function Get_Id: WideString;
    function Get_Time: WideString;
    function Get_ThId: WideString;
    function Get_Process: WideString;
    function Get_Icon: WideString;
    function Get_Parent: WideString;
    function Get_Node: IXMLNodeTypeList;
    function Get_Col2: WideString;
    function Get_ColValue: IXMLColumnList;
    function Get_ColTitle: IXMLColumnList;
    function Get_MainColumn: WideString;
    function Get_Member: IXMLMemberTypeList;
    function Get_FontDetail: IXMLFontDetailList;
    procedure Set_Id(Value: WideString);
    procedure Set_Time(Value: WideString);
    procedure Set_ThId(Value: WideString);
    procedure Set_Process(Value: WideString);
    procedure Set_Icon(Value: WideString);
    procedure Set_Parent(Value: WideString);
    procedure Set_Col2(Value: WideString);
    procedure Set_MainColumn(Value: WideString);
    { Methods & Properties }
    property Id: WideString read Get_Id write Set_Id;
    property Time: WideString read Get_Time write Set_Time;
    property ThId: WideString read Get_ThId write Set_ThId;
    property Process: WideString read Get_Process write Set_Process;
    property Icon: WideString read Get_Icon write Set_Icon;
    property Parent: WideString read Get_Parent write Set_Parent;
    property Node: IXMLNodeTypeList read Get_Node;
    property Col2: WideString read Get_Col2 write Set_Col2;
    property ColValue: IXMLColumnList read Get_ColValue;
    property ColTitle: IXMLColumnList read Get_ColTitle;
    property MainColumn: WideString read Get_MainColumn write Set_MainColumn;
    property Member: IXMLMemberTypeList read Get_Member;
    property FontDetail: IXMLFontDetailList read Get_FontDetail;
  end;

{ IXMLNodeTypeList }

  IXMLNodeTypeList = interface(IXMLNodeCollection)
    ['{447ED238-4318-4677-B2C4-3B980C8789E6}']
    { Methods & Properties }
    function Add: IXMLNodeType;
    function Insert(const Index: Integer): IXMLNodeType;
    function Get_Item(Index: Integer): IXMLNodeType;
    property Items[Index: Integer]: IXMLNodeType read Get_Item; default;
  end;

{ IXMLData }

  IXMLData = interface(IXMLNodeType)
    ['{ACA98BD9-31D9-45D3-8138-62DD566B4CA9}']
  end;

{ IXMLColumn }

  IXMLColumn = interface(IXMLNode)
    ['{7325C9DD-20E5-41D7-ABAF-E82681DECD88}']
    { Property Accessors }
    function Get_Order: Integer;
    procedure Set_Order(Value: Integer);
    { Methods & Properties }
    property Order: Integer read Get_Order write Set_Order;
  end;

{ IXMLColumnList }

  IXMLColumnList = interface(IXMLNodeCollection)
    ['{510DF71F-0193-4621-8732-C8F506ADF63A}']
    { Methods & Properties }
    function Add: IXMLColumn;
    function Insert(const Index: Integer): IXMLColumn;
    function Get_Item(Index: Integer): IXMLColumn;
    property Items[Index: Integer]: IXMLColumn read Get_Item; default;
  end;

{ IXMLMemberType }

  IXMLMemberType = interface(IXMLNode)
    ['{D25A0CE0-9994-4DBE-BD0B-965DDD29735A}']
    { Property Accessors }
    function Get_Member: IXMLMemberTypeList;
    function Get_ColB: WideString;
    function Get_ColC: WideString;
    function Get_FontDetail: IXMLFontDetailList;
    function Get_ViewerKind: Integer;
    procedure Set_ColB(Value: WideString);
    procedure Set_ColC(Value: WideString);
    procedure Set_ViewerKind(Value: Integer);
    { Methods & Properties }
    property Member: IXMLMemberTypeList read Get_Member;
    property ColB: WideString read Get_ColB write Set_ColB;
    property ColC: WideString read Get_ColC write Set_ColC;
    property FontDetail: IXMLFontDetailList read Get_FontDetail;
    property ViewerKind: Integer read Get_ViewerKind write Set_ViewerKind;
  end;

{ IXMLMemberTypeList }

  IXMLMemberTypeList = interface(IXMLNodeCollection)
    ['{AFAE73FA-8E64-48D0-A321-5194837D6E91}']
    { Methods & Properties }
    function Add: IXMLMemberType;
    function Insert(const Index: Integer): IXMLMemberType;
    function Get_Item(Index: Integer): IXMLMemberType;
    property Items[Index: Integer]: IXMLMemberType read Get_Item; default;
  end;

{ IXMLFontDetail }

  IXMLFontDetail = interface(IXMLNode)
    ['{D595963E-3E8D-48FF-BDA0-224BB09E237D}']
    { Property Accessors }
    function Get_ColId: Integer;
    function Get_Bold: Boolean;
    function Get_Italic: Boolean;
    function Get_Color: Integer;
    function Get_Size: Integer;
    function Get_Name: WideString;
    function Get_BackgroundColor: Integer;
    procedure Set_ColId(Value: Integer);
    procedure Set_Bold(Value: Boolean);
    procedure Set_Italic(Value: Boolean);
    procedure Set_Color(Value: Integer);
    procedure Set_Size(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_BackgroundColor(Value: Integer);
    { Methods & Properties }
    property ColId: Integer read Get_ColId write Set_ColId;
    property Bold: Boolean read Get_Bold write Set_Bold;
    property Italic: Boolean read Get_Italic write Set_Italic;
    property Color: Integer read Get_Color write Set_Color;
    property Size: Integer read Get_Size write Set_Size;
    property Name: WideString read Get_Name write Set_Name;
    property BackgroundColor: Integer read Get_BackgroundColor write Set_BackgroundColor;
  end;

{ IXMLFontDetailList }

  IXMLFontDetailList = interface(IXMLNodeCollection)
    ['{8E521A2B-2DED-4A85-918D-B92ED8D6BB74}']
    { Methods & Properties }
    function Add: IXMLFontDetail;
    function Insert(const Index: Integer): IXMLFontDetail;
    function Get_Item(Index: Integer): IXMLFontDetail;
    property Items[Index: Integer]: IXMLFontDetail read Get_Item; default;
  end;

{ Forward Decls }

  TXMLNodeType = class;
  TXMLNodeTypeList = class;
  TXMLData = class;
  TXMLColumn = class;
  TXMLColumnList = class;
  TXMLMemberType = class;
  TXMLMemberTypeList = class;
  TXMLFontDetail = class;
  TXMLFontDetailList = class;

{ TXMLNodeType }

  TXMLNodeType = class(TXMLNode, IXMLNodeType)
  private
    FNode: IXMLNodeTypeList;
    FColValue: IXMLColumnList;
    FColTitle: IXMLColumnList;
    FMember: IXMLMemberTypeList;
    FFontDetail: IXMLFontDetailList;
  protected
    { IXMLNodeType }
    function Get_Id: WideString;
    function Get_Time: WideString;
    function Get_ThId: WideString;
    function Get_Process: WideString;
    function Get_Icon: WideString;
    function Get_Parent: WideString;
    function Get_Node: IXMLNodeTypeList;
    function Get_Col2: WideString;
    function Get_ColValue: IXMLColumnList;
    function Get_ColTitle: IXMLColumnList;
    function Get_MainColumn: WideString;
    function Get_Member: IXMLMemberTypeList;
    function Get_FontDetail: IXMLFontDetailList;
    procedure Set_Id(Value: WideString);
    procedure Set_Time(Value: WideString);
    procedure Set_ThId(Value: WideString);
    procedure Set_Process(Value: WideString);
    procedure Set_Icon(Value: WideString);
    procedure Set_Parent(Value: WideString);
    procedure Set_Col2(Value: WideString);
    procedure Set_MainColumn(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLNodeTypeList }

  TXMLNodeTypeList = class(TXMLNodeCollection, IXMLNodeTypeList)
  protected
    { IXMLNodeTypeList }
    function Add: IXMLNodeType;
    function Insert(const Index: Integer): IXMLNodeType;
    function Get_Item(Index: Integer): IXMLNodeType;
  end;

{ TXMLData }

  TXMLData = class(TXMLNodeType, IXMLData)
  protected
    { IXMLData }
  end;

{ TXMLColumn }

  TXMLColumn = class(TXMLNode, IXMLColumn)
  protected
    { IXMLColumn }
    function Get_Order: Integer;
    procedure Set_Order(Value: Integer);
  end;

{ TXMLColumnList }

  TXMLColumnList = class(TXMLNodeCollection, IXMLColumnList)
  protected
    { IXMLColumnList }
    function Add: IXMLColumn;
    function Insert(const Index: Integer): IXMLColumn;
    function Get_Item(Index: Integer): IXMLColumn;
  end;

{ TXMLMemberType }

  TXMLMemberType = class(TXMLNode, IXMLMemberType)
  private
    FMember: IXMLMemberTypeList;
    FFontDetail: IXMLFontDetailList;
  protected
    { IXMLMemberType }
    function Get_Member: IXMLMemberTypeList;
    function Get_ColB: WideString;
    function Get_ColC: WideString;
    function Get_FontDetail: IXMLFontDetailList;
    function Get_ViewerKind: Integer;
    procedure Set_ColB(Value: WideString);
    procedure Set_ColC(Value: WideString);
    procedure Set_ViewerKind(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMemberTypeList }

  TXMLMemberTypeList = class(TXMLNodeCollection, IXMLMemberTypeList)
  protected
    { IXMLMemberTypeList }
    function Add: IXMLMemberType;
    function Insert(const Index: Integer): IXMLMemberType;
    function Get_Item(Index: Integer): IXMLMemberType;
  end;

{ TXMLFontDetail }

  TXMLFontDetail = class(TXMLNode, IXMLFontDetail)
  protected
    { IXMLFontDetail }
    function Get_ColId: Integer;
    function Get_Bold: Boolean;
    function Get_Italic: Boolean;
    function Get_Color: Integer;
    function Get_Size: Integer;
    function Get_Name: WideString;
    function Get_BackgroundColor: Integer;
    procedure Set_ColId(Value: Integer);
    procedure Set_Bold(Value: Boolean);
    procedure Set_Italic(Value: Boolean);
    procedure Set_Color(Value: Integer);
    procedure Set_Size(Value: Integer);
    procedure Set_Name(Value: WideString);
    procedure Set_BackgroundColor(Value: Integer);
  end;

{ TXMLFontDetailList }

  TXMLFontDetailList = class(TXMLNodeCollection, IXMLFontDetailList)
  protected
    { IXMLFontDetailList }
    function Add: IXMLFontDetail;
    function Insert(const Index: Integer): IXMLFontDetail;
    function Get_Item(Index: Integer): IXMLFontDetail;
  end;

{ Global Functions }

function GetData(Doc: IXMLDocument): IXMLData;
function LoadData(const FileName: WideString): IXMLData;
function NewData: IXMLData;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetData(Doc: IXMLDocument): IXMLData;
begin
  Result := Doc.GetDocBinding('Data', TXMLData, TargetNamespace) as IXMLData;
end;

function LoadData(const FileName: WideString): IXMLData;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('Data', TXMLData, TargetNamespace) as IXMLData;
end;

function NewData: IXMLData;
begin
  Result := NewXMLDocument.GetDocBinding('Data', TXMLData, TargetNamespace) as IXMLData;
end;

{ TXMLNodeType }

procedure TXMLNodeType.AfterConstruction;
begin
  RegisterChildNode('Node', TXMLNodeType);
  RegisterChildNode('ColValue', TXMLColumn);
  RegisterChildNode('ColTitle', TXMLColumn);
  RegisterChildNode('Member', TXMLMemberType);
  RegisterChildNode('FontDetail', TXMLFontDetail);
  FNode := CreateCollection(TXMLNodeTypeList, IXMLNodeType, 'Node') as IXMLNodeTypeList;
  FColValue := CreateCollection(TXMLColumnList, IXMLColumn, 'ColValue') as IXMLColumnList;
  FColTitle := CreateCollection(TXMLColumnList, IXMLColumn, 'ColTitle') as IXMLColumnList;
  FMember := CreateCollection(TXMLMemberTypeList, IXMLMemberType, 'Member') as IXMLMemberTypeList;
  FFontDetail := CreateCollection(TXMLFontDetailList, IXMLFontDetail, 'FontDetail') as IXMLFontDetailList;
  inherited;
end;

function TXMLNodeType.Get_Id: WideString;
begin
  Result := AttributeNodes['Id'].Text;
end;

procedure TXMLNodeType.Set_Id(Value: WideString);
begin
  SetAttribute('Id', Value);
end;

function TXMLNodeType.Get_Time: WideString;
begin
  Result := AttributeNodes['Time'].Text;
end;

procedure TXMLNodeType.Set_Time(Value: WideString);
begin
  SetAttribute('Time', Value);
end;

function TXMLNodeType.Get_ThId: WideString;
begin
  Result := AttributeNodes['ThId'].Text;
end;

procedure TXMLNodeType.Set_ThId(Value: WideString);
begin
  SetAttribute('ThId', Value);
end;

function TXMLNodeType.Get_Process: WideString;
begin
  Result := AttributeNodes['Process'].Text;
end;

procedure TXMLNodeType.Set_Process(Value: WideString);
begin
  SetAttribute('Process', Value);
end;

function TXMLNodeType.Get_Icon: WideString;
begin
  Result := AttributeNodes['Icon'].Text;
end;

procedure TXMLNodeType.Set_Icon(Value: WideString);
begin
  SetAttribute('Icon', Value);
end;

function TXMLNodeType.Get_Parent: WideString;
begin
  Result := AttributeNodes['Parent'].Text;
end;

procedure TXMLNodeType.Set_Parent(Value: WideString);
begin
  SetAttribute('Parent', Value);
end;

function TXMLNodeType.Get_Node: IXMLNodeTypeList;
begin
  Result := FNode;
end;

function TXMLNodeType.Get_Col2: WideString;
begin
  Result := ChildNodes['Col2'].Text;
end;

procedure TXMLNodeType.Set_Col2(Value: WideString);
begin
  ChildNodes['Col2'].NodeValue := Value;
end;

function TXMLNodeType.Get_ColValue: IXMLColumnList;
begin
  Result := FColValue;
end;

function TXMLNodeType.Get_ColTitle: IXMLColumnList;
begin
  Result := FColTitle;
end;

function TXMLNodeType.Get_MainColumn: WideString;
begin
  Result := ChildNodes['MainColumn'].Text;
end;

procedure TXMLNodeType.Set_MainColumn(Value: WideString);
begin
  ChildNodes['MainColumn'].NodeValue := Value;
end;

function TXMLNodeType.Get_Member: IXMLMemberTypeList;
begin
  Result := FMember;
end;

function TXMLNodeType.Get_FontDetail: IXMLFontDetailList;
begin
  Result := FFontDetail;
end;

{ TXMLNodeTypeList }

function TXMLNodeTypeList.Add: IXMLNodeType;
begin
  Result := AddItem(-1) as IXMLNodeType;
end;

function TXMLNodeTypeList.Insert(const Index: Integer): IXMLNodeType;
begin
  Result := AddItem(Index) as IXMLNodeType;
end;
function TXMLNodeTypeList.Get_Item(Index: Integer): IXMLNodeType;
begin
  Result := List[Index] as IXMLNodeType;
end;

{ TXMLData }

{ TXMLColumn }

function TXMLColumn.Get_Order: Integer;
begin
  Result := AttributeNodes['Order'].NodeValue;
end;

procedure TXMLColumn.Set_Order(Value: Integer);
begin
  SetAttribute('Order', Value);
end;

{ TXMLColumnList }

function TXMLColumnList.Add: IXMLColumn;
begin
  Result := AddItem(-1) as IXMLColumn;
end;

function TXMLColumnList.Insert(const Index: Integer): IXMLColumn;
begin
  Result := AddItem(Index) as IXMLColumn;
end;
function TXMLColumnList.Get_Item(Index: Integer): IXMLColumn;
begin
  Result := List[Index] as IXMLColumn;
end;

{ TXMLMemberType }

procedure TXMLMemberType.AfterConstruction;
begin
  RegisterChildNode('Member', TXMLMemberType);
  RegisterChildNode('FontDetail', TXMLFontDetail);
  FMember := CreateCollection(TXMLMemberTypeList, IXMLMemberType, 'Member') as IXMLMemberTypeList;
  FFontDetail := CreateCollection(TXMLFontDetailList, IXMLFontDetail, 'FontDetail') as IXMLFontDetailList;
  inherited;
end;

function TXMLMemberType.Get_Member: IXMLMemberTypeList;
begin
  Result := FMember;
end;

function TXMLMemberType.Get_ColB: WideString;
begin
  Result := ChildNodes['ColB'].Text;
end;

procedure TXMLMemberType.Set_ColB(Value: WideString);
begin
  ChildNodes['ColB'].NodeValue := Value;
end;

function TXMLMemberType.Get_ColC: WideString;
begin
  Result := ChildNodes['ColC'].Text;
end;

procedure TXMLMemberType.Set_ColC(Value: WideString);
begin
  ChildNodes['ColC'].NodeValue := Value;
end;

function TXMLMemberType.Get_FontDetail: IXMLFontDetailList;
begin
  Result := FFontDetail;
end;

function TXMLMemberType.Get_ViewerKind: Integer;
begin
  Result := ChildNodes['ViewerKind'].NodeValue;
end;

procedure TXMLMemberType.Set_ViewerKind(Value: Integer);
begin
  ChildNodes['ViewerKind'].NodeValue := Value;
end;

{ TXMLMemberTypeList }

function TXMLMemberTypeList.Add: IXMLMemberType;
begin
  Result := AddItem(-1) as IXMLMemberType;
end;

function TXMLMemberTypeList.Insert(const Index: Integer): IXMLMemberType;
begin
  Result := AddItem(Index) as IXMLMemberType;
end;
function TXMLMemberTypeList.Get_Item(Index: Integer): IXMLMemberType;
begin
  Result := List[Index] as IXMLMemberType;
end;

{ TXMLFontDetail }

function TXMLFontDetail.Get_ColId: Integer;
begin
  Result := AttributeNodes['ColId'].NodeValue;
end;

procedure TXMLFontDetail.Set_ColId(Value: Integer);
begin
  SetAttribute('ColId', Value);
end;

function TXMLFontDetail.Get_Bold: Boolean;
begin
  Result := AttributeNodes['Bold'].NodeValue;
end;

procedure TXMLFontDetail.Set_Bold(Value: Boolean);
begin
  SetAttribute('Bold', Value);
end;

function TXMLFontDetail.Get_Italic: Boolean;
begin
  Result := AttributeNodes['Italic'].NodeValue;
end;

procedure TXMLFontDetail.Set_Italic(Value: Boolean);
begin
  SetAttribute('Italic', Value);
end;

function TXMLFontDetail.Get_Color: Integer;
begin
  Result := AttributeNodes['Color'].NodeValue;
end;

procedure TXMLFontDetail.Set_Color(Value: Integer);
begin
  SetAttribute('Color', Value);
end;

function TXMLFontDetail.Get_Size: Integer;
begin
  Result := AttributeNodes['Size'].NodeValue;
end;

procedure TXMLFontDetail.Set_Size(Value: Integer);
begin
  SetAttribute('Size', Value);
end;

function TXMLFontDetail.Get_Name: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLFontDetail.Set_Name(Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLFontDetail.Get_BackgroundColor: Integer;
begin
  Result := AttributeNodes['BackgroundColor'].NodeValue;
end;

procedure TXMLFontDetail.Set_BackgroundColor(Value: Integer);
begin
  SetAttribute('BackgroundColor', Value);
end;

{ TXMLFontDetailList }

function TXMLFontDetailList.Add: IXMLFontDetail;
begin
  Result := AddItem(-1) as IXMLFontDetail;
end;

function TXMLFontDetailList.Insert(const Index: Integer): IXMLFontDetail;
begin
  Result := AddItem(Index) as IXMLFontDetail;
end;
function TXMLFontDetailList.Get_Item(Index: Integer): IXMLFontDetail;
begin
  Result := List[Index] as IXMLFontDetail;
end;

end.