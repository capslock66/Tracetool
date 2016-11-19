unit unt_importApp;


interface

uses
  classes,  forms,  sysutils, controls, menus, typInfo,
  Unt_XmlApp;    // needed to load properties from saved file

procedure ImportApp (filename :string) ;
procedure RestoreXmlObject(XmlObject: IXMLContainerType; obj : TObject; ident : string);

implementation

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure ImportApp (filename :string) ;
var
   XMLData : IXMLData ;
begin
   XMLData := LoadData (filename) ;
   RestoreXmlObject (XMLData, Screen ,'');
end ;

//------------------------------------------------------------------------------
// RestoreXmlObject is recursive
procedure RestoreXmlObject (XmlObject : IXMLContainerType ; Obj : TObject; ident : string);
var
   c,d : integer ;
   MasterComponentName : string ;
   foundIndex: integer ;
   PropInfo : ppropinfo ;
   intPropValue : integer ;
   Collection : TCollection ;
begin

   ident := ident + '   ' ;

   if obj is TComponent then
      MasterComponentName := TComponent (obj).Name
   else
      MasterComponentName := '(not TComponent)' ;

   MasterComponentName := MasterComponentName + ' : ' + Obj.className ;

   // <Prop> Parent is any Persistent object (note : not means TPersistant, little difference)
   for c := 0 to XmlObject.Prop.Count -1 do begin
      //senddebug (ident + 'Restore Prop ' + XmlObject.Prop[c].Name + ' of ' + MasterComponentName +
      //           ' to ' + XmlObject.prop[c].Value + XmlObject.prop[c].Txt) ;

      // if attribute Value then it's an integer value, else use the <txt> tag for strings
      PropInfo := GetPropInfo(obj,XmlObject.Prop[c].Name,[tkInteger,tkInt64,tkChar,tkString,tkWChar,tkLString,tkWString,tkClass]);
      if PropInfo <> nil then begin
         if XmlObject.prop[c].Value <> '' then  // Value is for integer value
            SetOrdProp (obj, PropInfo, strtointdef(XmlObject.prop[c].Value,0))
         else if XmlObject.prop[c].Txt <> '' then // TXT is for string prop
            SetStrProp(obj, propinfo, XmlObject.prop[c].Txt) ;

         if PropInfo^.PropType^.Kind = tkClass then begin
            intPropValue := GetOrdProp(obj, PropInfo) ;
            if intPropValue <> 0 then  // the property don't point to a nil TObject
               RestoreXmlObject (XmlObject.prop[c], TObject (intPropValue) ,ident);
         end ;
      end else begin
       //senddebug (ident + 'Prop not found !!!!!!!!!!!!!') ;
     end ;
   end ;

   // <Form> Parent is TScreen
   for c := 0 to XmlObject.Form.Count -1 do begin
      //senddebug (ident + 'Restore form ' + XmlObject.Form[c].Name + ' of ' + MasterComponentName ) ;
      //search form
      foundIndex := -1 ;
      for d:= 0 to TScreen(obj).FormCount -1 do begin
         if TScreen(obj).Forms[d].Name = XmlObject.Form[c].Name then begin
            foundIndex := d;
            break ;
         end ;
      end ;
      if foundIndex <> -1 then begin
         // recursive : restore XmlObject type <form> with Object type Tform
         RestoreXmlObject (XmlObject.Form[c], TScreen(obj).Forms[d],ident);
         //Senddebug ('------------');
      end else begin
         //Senddebug (ident + 'Form not found !!!!!!!!!!!!!!!!!!!!');
      end ;
   end ;

   // <Control> Parent is a TWinControl object
   for c := 0 to XmlObject.Control.Count -1 do begin
      //senddebug (ident + 'Restore Control ' + XmlObject.Control[c].Name + ' of ' + MasterComponentName ) ;
      //search control
      foundIndex := -1 ;
      for d := 0 to TWinControl(Obj).ControlCount  -1 do begin
         if TWinControl(Obj).Controls[d].Name = XmlObject.Control[c].Name then begin
            foundIndex := d;
            break ;
         end ;
      end ;
      if foundIndex <> -1 then begin
         // recursive : restore XmlObject type <control> with Object type TWinControl
         RestoreXmlObject (XmlObject.Control[c], TWinControl(Obj).Controls[d],ident);
      end else begin
         //Senddebug (ident + 'control not found !!!!!!!!!!!!!!!!!!!!');
      end ;
   end ;

   // <Component> : Parent is a TComponent object
   for c := 0 to XmlObject.Component.Count -1 do begin
      //senddebug (ident + 'Restore Component ' + XmlObject.Component[c].Name + ' of ' + MasterComponentName ) ;
      //search component
      foundIndex := -1 ;
      for d := 0 to TComponent(Obj).ComponentCount -1 do begin
         if TComponent(Obj).Components[d].Name = XmlObject.Component[c].Name then begin
            foundIndex := d;
            break ;
         end ;
      end ;
      if foundIndex <> -1 then begin
         // recursive : restore XmlObject type <control> with Object type TWinControl
         RestoreXmlObject (XmlObject.Component[c], TComponent(Obj).Components[d],ident);
      end else begin
         //Senddebug (ident + 'component not found !!!!!!!!!!!!!!!!!!!!');
      end ;
   end ;

   // <Menu> : Parent is a TMainMenu or a TPopupMenu (each are a TMenu object)
   for c := 0 to XmlObject.Menu.Count -1 do begin
      //senddebug (ident + 'Restore Top Menu ' + XmlObject.Menu[c].Name + ' of ' + MasterComponentName ) ;
      foundIndex := -1 ;
      for d := 0 to TMenu(obj).Items.Count-1 do begin
         if TMenu(obj).Items[d].Name = XmlObject.menu[c].Name then begin
            foundindex := d ;
            break ;
         end ;
      end ;
      if foundIndex <> -1 then begin
         // recursive : restore XmlObject type <control> with Object type TWinControl
         RestoreXmlObject (XmlObject.Menu[c], TMenu(obj).Items[d],ident);
      end else begin
         //Senddebug (ident + 'Top menu not found !!!!!!!!!!!!!!!!!!!!');
      end ;
   end ;

   // <ItemMenu> : Parent is a TMenuItem
   for c := 0 to XmlObject.ItemMenu.Count -1 do begin
      //senddebug (ident + 'Restore sub Menu ' + XmlObject.ItemMenu[c].Name + ' of ' + MasterComponentName ) ;
      foundIndex := -1 ;
      for d := 0 to TMenuItem (obj).Count-1 do begin
         if TMenuItem(obj).Items[d].Name = XmlObject.ItemMenu[c].Name then begin
            foundindex := d ;
            break ;
         end ;
      end ;
      if foundIndex <> -1 then begin
         // recursive : restore XmlObject type <control> with Object type TWinControl
         RestoreXmlObject (XmlObject.ItemMenu[c], TMenuItem(obj).Items[d],ident);
      end else begin
         //Senddebug (ident + 'Sub menu not found !!!!!!!!!!!!!!!!!!!!');
      end ;
   end ;

   // <ItemCollection> : Parent is a simple persistent object.
   // we need to get the Sub Object property (type class) of the object that match the item and recursive call the function
   for c := 0 to XmlObject.ItemCollection.Count -1 do begin
      //senddebug (ident + 'Restore ItemCollection ' + XmlObject.ItemCollection[c].Name + ' of  ' + MasterComponentName ) ;
      if not (obj is TCollection) then begin
         //senddebug ('Master object is NOT a tcollection !!!');
         continue ;
      end ;
      Collection := TCollection (obj) ;
      d := strtointdef(XmlObject.ItemCollection[c].Name,-1);
      if (d = -1) or (d >= Collection.Count) then begin
         //senddebug ('ItemCollection/@Name is not valid : must be integer less than ' + inttostr (Collection.Count));
         continue ;
      end ;
      RestoreXmlObject (XmlObject.ItemCollection[c], Collection.Items[d],ident);
   end ;
end ;



end.
