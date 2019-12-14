// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.WPF

#nowarn "59" // cast always holds
#nowarn "66" // cast always holds
#nowarn "67" // cast always holds

open Fabulous

module ViewAttributes =
    let HorizontalAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("HorizontalAlignment")
    let MarginAttribKey : AttributeKey<_> = AttributeKey<_>("Margin")
    let VerticalAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("VerticalAlignment")
    let WidthAttribKey : AttributeKey<_> = AttributeKey<_>("Width")
    let ContentControlContentAttribKey : AttributeKey<_> = AttributeKey<_>("ContentControlContent")
    let TitleAttribKey : AttributeKey<_> = AttributeKey<_>("Title")
    let CommandAttribKey : AttributeKey<_> = AttributeKey<_>("Command")
    let CommandCanExecuteAttribKey : AttributeKey<_> = AttributeKey<_>("CommandCanExecute")
    let ButtonContentAttribKey : AttributeKey<_> = AttributeKey<_>("ButtonContent")
    let CheckedAttribKey : AttributeKey<_> = AttributeKey<_>("Checked")
    let UncheckedAttribKey : AttributeKey<_> = AttributeKey<_>("Unchecked")
    let IsCheckedAttribKey : AttributeKey<_> = AttributeKey<_>("IsChecked")
    let TextAttribKey : AttributeKey<_> = AttributeKey<_>("Text")
    let TextAlignmentAttribKey : AttributeKey<_> = AttributeKey<_>("TextAlignment")
    let ValueChangedAttribKey : AttributeKey<_> = AttributeKey<_>("ValueChanged")
    let MinimumAttribKey : AttributeKey<_> = AttributeKey<_>("Minimum")
    let MaximumAttribKey : AttributeKey<_> = AttributeKey<_>("Maximum")
    let ValueAttribKey : AttributeKey<_> = AttributeKey<_>("Value")
    let ChildrenAttribKey : AttributeKey<_> = AttributeKey<_>("Children")
    let OrientationAttribKey : AttributeKey<_> = AttributeKey<_>("Orientation")
    let ChildAttribKey : AttributeKey<_> = AttributeKey<_>("Child")
    let CornerRadiusAttribKey : AttributeKey<_> = AttributeKey<_>("CornerRadius")
    let BorderThicknessAttribKey : AttributeKey<_> = AttributeKey<_>("BorderThickness")
    let PaddingAttribKey : AttributeKey<_> = AttributeKey<_>("Padding")
    let BorderBrushAttribKey : AttributeKey<_> = AttributeKey<_>("BorderBrush")
    let BackgroundAttribKey : AttributeKey<_> = AttributeKey<_>("Background")
    let GroupNameAttribKey : AttributeKey<_> = AttributeKey<_>("GroupName")
    let RadioButtonContentAttribKey : AttributeKey<_> = AttributeKey<_>("RadioButtonContent")
    let LastChildFillAttribKey : AttributeKey<_> = AttributeKey<_>("LastChildFill")

type ViewBuilders() =
    /// Builds the attributes for a UIElement in the view
    static member inline BuildUIElement(attribCount: int) = 
        let attribBuilder = new AttributesBuilder(attribCount)
        attribBuilder

    static member UpdateUIElement (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.UIElement) = 
        ()

    /// Builds the attributes for a FrameworkElement in the view
    static member inline BuildFrameworkElement(attribCount: int,
                                               ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                               ?margin: System.Windows.Thickness,
                                               ?verticalAlignment: System.Windows.VerticalAlignment,
                                               ?width: float) = 

        let attribCount = match horizontalAlignment with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match margin with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match verticalAlignment with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match width with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildUIElement(attribCount)
        match horizontalAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.HorizontalAlignmentAttribKey, (v)) 
        match margin with None -> () | Some v -> attribBuilder.Add(ViewAttributes.MarginAttribKey, (v)) 
        match verticalAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.VerticalAlignmentAttribKey, (v)) 
        match width with None -> () | Some v -> attribBuilder.Add(ViewAttributes.WidthAttribKey, (v)) 
        attribBuilder

    static member UpdateFrameworkElement (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.FrameworkElement) = 
        let mutable prevHorizontalAlignmentOpt = ValueNone
        let mutable currHorizontalAlignmentOpt = ValueNone
        let mutable prevMarginOpt = ValueNone
        let mutable currMarginOpt = ValueNone
        let mutable prevVerticalAlignmentOpt = ValueNone
        let mutable currVerticalAlignmentOpt = ValueNone
        let mutable prevWidthOpt = ValueNone
        let mutable currWidthOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.HorizontalAlignmentAttribKey.KeyValue then 
                currHorizontalAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.HorizontalAlignment)
            if kvp.Key = ViewAttributes.MarginAttribKey.KeyValue then 
                currMarginOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
            if kvp.Key = ViewAttributes.VerticalAlignmentAttribKey.KeyValue then 
                currVerticalAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.VerticalAlignment)
            if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                currWidthOpt <- ValueSome (kvp.Value :?> float)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.HorizontalAlignmentAttribKey.KeyValue then 
                    prevHorizontalAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.HorizontalAlignment)
                if kvp.Key = ViewAttributes.MarginAttribKey.KeyValue then 
                    prevMarginOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
                if kvp.Key = ViewAttributes.VerticalAlignmentAttribKey.KeyValue then 
                    prevVerticalAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.VerticalAlignment)
                if kvp.Key = ViewAttributes.WidthAttribKey.KeyValue then 
                    prevWidthOpt <- ValueSome (kvp.Value :?> float)
        // Update inherited members
        ViewBuilders.UpdateUIElement (prevOpt, curr, target)
        // Update properties
        match prevHorizontalAlignmentOpt, currHorizontalAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.HorizontalAlignment <-  currValue
        | ValueSome _, ValueNone -> target.HorizontalAlignment <- System.Windows.HorizontalAlignment.Stretch
        | ValueNone, ValueNone -> ()
        match prevMarginOpt, currMarginOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Margin <-  currValue
        | ValueSome _, ValueNone -> target.Margin <- System.Windows.Thickness(0.)
        | ValueNone, ValueNone -> ()
        match prevVerticalAlignmentOpt, currVerticalAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.VerticalAlignment <-  currValue
        | ValueSome _, ValueNone -> target.VerticalAlignment <- System.Windows.VerticalAlignment.Stretch
        | ValueNone, ValueNone -> ()
        match prevWidthOpt, currWidthOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Width <-  currValue
        | ValueSome _, ValueNone -> target.Width <- System.Double.NaN
        | ValueNone, ValueNone -> ()

    /// Builds the attributes for a ContentControl in the view
    static member inline BuildContentControl(attribCount: int,
                                             ?content: ViewElement,
                                             ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                             ?margin: System.Windows.Thickness,
                                             ?verticalAlignment: System.Windows.VerticalAlignment,
                                             ?width: float) = 

        let attribCount = match content with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match content with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ContentControlContentAttribKey, (v)) 
        attribBuilder

    static member CreateContentControl () : System.Windows.Controls.ContentControl =
        new System.Windows.Controls.ContentControl()

    static member UpdateContentControl (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.ContentControl) = 
        let mutable prevContentControlContentOpt = ValueNone
        let mutable currContentControlContentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ContentControlContentAttribKey.KeyValue then 
                currContentControlContentOpt <- ValueSome (kvp.Value :?> ViewElement)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ContentControlContentAttribKey.KeyValue then 
                    prevContentControlContentOpt <- ValueSome (kvp.Value :?> ViewElement)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevContentControlContentOpt, currContentControlContentOpt with
        // For structured objects, dependsOn on reference equality
        | ValueSome prevValue, ValueSome newValue when identical prevValue newValue -> ()
        | ValueSome prevValue, ValueSome newValue when canReuseView prevValue newValue ->
            newValue.UpdateIncremental(prevValue, target.Content)
        | _, ValueSome newValue ->
            target.Content <- (newValue.Create() :?> obj)
        | ValueSome _, ValueNone ->
            target.Content <- null
        | ValueNone, ValueNone -> ()

    static member inline ConstructContentControl(?content: ViewElement,
                                                 ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                                 ?margin: System.Windows.Thickness,
                                                 ?verticalAlignment: System.Windows.VerticalAlignment,
                                                 ?width: float) = 

        let attribBuilder = ViewBuilders.BuildContentControl(0,
                               ?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.ContentControl>(ViewBuilders.CreateContentControl, (fun prevOpt curr target -> ViewBuilders.UpdateContentControl(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a Window in the view
    static member inline BuildWindow(attribCount: int,
                                     ?title: string,
                                     ?content: ViewElement,
                                     ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                     ?margin: System.Windows.Thickness,
                                     ?verticalAlignment: System.Windows.VerticalAlignment,
                                     ?width: float) = 

        let attribCount = match title with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildContentControl(attribCount, ?content=content, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match title with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TitleAttribKey, (v)) 
        attribBuilder

    static member CreateWindow () : System.Windows.Window =
        new System.Windows.Window()

    static member UpdateWindow (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Window) = 
        let mutable prevTitleOpt = ValueNone
        let mutable currTitleOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                currTitleOpt <- ValueSome (kvp.Value :?> string)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                    prevTitleOpt <- ValueSome (kvp.Value :?> string)
        // Update inherited members
        ViewBuilders.UpdateContentControl (prevOpt, curr, target)
        // Update properties
        match prevTitleOpt, currTitleOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Title <-  currValue
        | ValueSome _, ValueNone -> target.Title <- System.String.Empty
        | ValueNone, ValueNone -> ()

    static member inline ConstructWindow(?title: string,
                                         ?content: ViewElement,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float) = 

        let attribBuilder = ViewBuilders.BuildWindow(0,
                               ?title=title,
                               ?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Window>(ViewBuilders.CreateWindow, (fun prevOpt curr target -> ViewBuilders.UpdateWindow(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a ButtonBase in the view
    static member inline BuildButtonBase(attribCount: int,
                                         ?command: unit -> unit,
                                         ?commandCanExecute: bool,
                                         ?content: ViewElement,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float) = 

        let attribCount = match command with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match commandCanExecute with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildContentControl(attribCount, ?content=content, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match command with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CommandAttribKey, (v)) 
        match commandCanExecute with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CommandCanExecuteAttribKey, (v)) 
        attribBuilder

    static member UpdateButtonBase (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Primitives.ButtonBase) = 
        let mutable prevCommandOpt = ValueNone
        let mutable currCommandOpt = ValueNone
        let mutable prevCommandCanExecuteOpt = ValueNone
        let mutable currCommandCanExecuteOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.CommandAttribKey.KeyValue then 
                currCommandOpt <- ValueSome (kvp.Value :?> unit -> unit)
            if kvp.Key = ViewAttributes.CommandCanExecuteAttribKey.KeyValue then 
                currCommandCanExecuteOpt <- ValueSome (kvp.Value :?> bool)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.CommandAttribKey.KeyValue then 
                    prevCommandOpt <- ValueSome (kvp.Value :?> unit -> unit)
                if kvp.Key = ViewAttributes.CommandCanExecuteAttribKey.KeyValue then 
                    prevCommandCanExecuteOpt <- ValueSome (kvp.Value :?> bool)
        // Update inherited members
        ViewBuilders.UpdateContentControl (prevOpt, curr, target)
        // Update properties
        (fun _ _ _ -> ()) prevCommandOpt currCommandOpt target
        ViewUpdaters.updateCommand prevCommandOpt currCommandOpt (fun _target -> ()) (fun (target: System.Windows.Controls.Primitives.ButtonBase) cmd -> target.Command <- cmd) prevCommandCanExecuteOpt currCommandCanExecuteOpt target

    /// Builds the attributes for a Button in the view
    static member inline BuildButton(attribCount: int,
                                     ?content: string,
                                     ?command: unit -> unit,
                                     ?commandCanExecute: bool,
                                     ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                     ?margin: System.Windows.Thickness,
                                     ?verticalAlignment: System.Windows.VerticalAlignment,
                                     ?width: float) = 

        let attribCount = match content with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildButtonBase(attribCount, ?command=command, ?commandCanExecute=commandCanExecute, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, 
                                                         ?width=width)
        match content with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ButtonContentAttribKey, (v)) 
        attribBuilder

    static member CreateButton () : System.Windows.Controls.Button =
        new System.Windows.Controls.Button()

    static member UpdateButton (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Button) = 
        let mutable prevButtonContentOpt = ValueNone
        let mutable currButtonContentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ButtonContentAttribKey.KeyValue then 
                currButtonContentOpt <- ValueSome (kvp.Value :?> string)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ButtonContentAttribKey.KeyValue then 
                    prevButtonContentOpt <- ValueSome (kvp.Value :?> string)
        // Update inherited members
        ViewBuilders.UpdateButtonBase (prevOpt, curr, target)
        // Update properties
        match prevButtonContentOpt, currButtonContentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Content <-  currValue
        | ValueSome _, ValueNone -> target.Content <- null
        | ValueNone, ValueNone -> ()

    static member inline ConstructButton(?content: string,
                                         ?command: unit -> unit,
                                         ?commandCanExecute: bool,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float) = 

        let attribBuilder = ViewBuilders.BuildButton(0,
                               ?content=content,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.Button>(ViewBuilders.CreateButton, (fun prevOpt curr target -> ViewBuilders.UpdateButton(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a ToggleButton in the view
    static member inline BuildToggleButton(attribCount: int,
                                           ?isChecked: bool option,
                                           ?command: unit -> unit,
                                           ?commandCanExecute: bool,
                                           ?content: ViewElement,
                                           ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                           ?margin: System.Windows.Thickness,
                                           ?verticalAlignment: System.Windows.VerticalAlignment,
                                           ?width: float,
                                           ?checked: unit -> unit,
                                           ?unchecked: unit -> unit) = 

        let attribCount = match isChecked with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match checked with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match unchecked with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildButtonBase(attribCount, ?command=command, ?commandCanExecute=commandCanExecute, ?content=content, ?horizontalAlignment=horizontalAlignment, ?margin=margin, 
                                                         ?verticalAlignment=verticalAlignment, ?width=width)
        match isChecked with None -> () | Some v -> attribBuilder.Add(ViewAttributes.IsCheckedAttribKey, (v)) 
        match checked with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CheckedAttribKey, (fun f -> System.Windows.RoutedEventHandler(fun _sender _args -> f()))(v)) 
        match unchecked with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UncheckedAttribKey, (fun f -> System.Windows.RoutedEventHandler(fun _sender _args -> f()))(v)) 
        attribBuilder

    static member UpdateToggleButton (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Primitives.ToggleButton) = 
        let mutable prevCheckedOpt = ValueNone
        let mutable currCheckedOpt = ValueNone
        let mutable prevUncheckedOpt = ValueNone
        let mutable currUncheckedOpt = ValueNone
        let mutable prevIsCheckedOpt = ValueNone
        let mutable currIsCheckedOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.CheckedAttribKey.KeyValue then 
                currCheckedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedEventHandler)
            if kvp.Key = ViewAttributes.UncheckedAttribKey.KeyValue then 
                currUncheckedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedEventHandler)
            if kvp.Key = ViewAttributes.IsCheckedAttribKey.KeyValue then 
                currIsCheckedOpt <- ValueSome (kvp.Value :?> bool option)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.CheckedAttribKey.KeyValue then 
                    prevCheckedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedEventHandler)
                if kvp.Key = ViewAttributes.UncheckedAttribKey.KeyValue then 
                    prevUncheckedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedEventHandler)
                if kvp.Key = ViewAttributes.IsCheckedAttribKey.KeyValue then 
                    prevIsCheckedOpt <- ValueSome (kvp.Value :?> bool option)
        // Unsubscribe previous event handlers
        let shouldUpdateChecked = not ((identical prevCheckedOpt currCheckedOpt))
        if shouldUpdateChecked then
            match prevCheckedOpt with
            | ValueSome prevValue -> target.Checked.RemoveHandler(prevValue)
            | ValueNone -> ()
        let shouldUpdateUnchecked = not ((identical prevUncheckedOpt currUncheckedOpt))
        if shouldUpdateUnchecked then
            match prevUncheckedOpt with
            | ValueSome prevValue -> target.Unchecked.RemoveHandler(prevValue)
            | ValueNone -> ()
        // Update inherited members
        ViewBuilders.UpdateButtonBase (prevOpt, curr, target)
        // Update properties
        match prevIsCheckedOpt, currIsCheckedOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.IsChecked <- Option.toNullable currValue
        | ValueSome _, ValueNone -> target.IsChecked <- System.Nullable<bool>(true)
        | ValueNone, ValueNone -> ()
        // Subscribe new event handlers
        if shouldUpdateChecked then
            match currCheckedOpt with
            | ValueSome currValue -> target.Checked.AddHandler(currValue)
            | ValueNone -> ()
        if shouldUpdateUnchecked then
            match currUncheckedOpt with
            | ValueSome currValue -> target.Unchecked.AddHandler(currValue)
            | ValueNone -> ()

    /// Builds the attributes for a CheckBox in the view
    static member inline BuildCheckBox(attribCount: int,
                                       ?isChecked: bool option,
                                       ?command: unit -> unit,
                                       ?commandCanExecute: bool,
                                       ?content: ViewElement,
                                       ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                       ?margin: System.Windows.Thickness,
                                       ?verticalAlignment: System.Windows.VerticalAlignment,
                                       ?width: float,
                                       ?checked: unit -> unit,
                                       ?unchecked: unit -> unit) = 
        let attribBuilder = ViewBuilders.BuildToggleButton(attribCount, ?isChecked=isChecked, ?command=command, ?commandCanExecute=commandCanExecute, ?content=content, ?horizontalAlignment=horizontalAlignment, 
                                                           ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width, ?checked=checked, ?unchecked=unchecked)
        attribBuilder

    static member CreateCheckBox () : System.Windows.Controls.CheckBox =
        new System.Windows.Controls.CheckBox()

    static member UpdateCheckBox (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.CheckBox) = 
        ViewBuilders.UpdateToggleButton (prevOpt, curr, target)

    static member inline ConstructCheckBox(?isChecked: bool option,
                                           ?command: unit -> unit,
                                           ?commandCanExecute: bool,
                                           ?content: ViewElement,
                                           ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                           ?margin: System.Windows.Thickness,
                                           ?verticalAlignment: System.Windows.VerticalAlignment,
                                           ?width: float,
                                           ?checked: unit -> unit,
                                           ?unchecked: unit -> unit) = 

        let attribBuilder = ViewBuilders.BuildCheckBox(0,
                               ?isChecked=isChecked,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width,
                               ?checked=checked,
                               ?unchecked=unchecked)

        ViewElement.Create<System.Windows.Controls.CheckBox>(ViewBuilders.CreateCheckBox, (fun prevOpt curr target -> ViewBuilders.UpdateCheckBox(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a TextBlock in the view
    static member inline BuildTextBlock(attribCount: int,
                                        ?text: string,
                                        ?textAlignment: System.Windows.TextAlignment,
                                        ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                        ?margin: System.Windows.Thickness,
                                        ?verticalAlignment: System.Windows.VerticalAlignment,
                                        ?width: float) = 

        let attribCount = match text with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match textAlignment with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match text with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TextAttribKey, (v)) 
        match textAlignment with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TextAlignmentAttribKey, (v)) 
        attribBuilder

    static member CreateTextBlock () : System.Windows.Controls.TextBlock =
        new System.Windows.Controls.TextBlock()

    static member UpdateTextBlock (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.TextBlock) = 
        let mutable prevTextOpt = ValueNone
        let mutable currTextOpt = ValueNone
        let mutable prevTextAlignmentOpt = ValueNone
        let mutable currTextAlignmentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                currTextOpt <- ValueSome (kvp.Value :?> string)
            if kvp.Key = ViewAttributes.TextAlignmentAttribKey.KeyValue then 
                currTextAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.TextAlignment)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TextAttribKey.KeyValue then 
                    prevTextOpt <- ValueSome (kvp.Value :?> string)
                if kvp.Key = ViewAttributes.TextAlignmentAttribKey.KeyValue then 
                    prevTextAlignmentOpt <- ValueSome (kvp.Value :?> System.Windows.TextAlignment)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevTextOpt, currTextOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Text <-  currValue
        | ValueSome _, ValueNone -> target.Text <- System.String.Empty
        | ValueNone, ValueNone -> ()
        match prevTextAlignmentOpt, currTextAlignmentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.TextAlignment <-  currValue
        | ValueSome _, ValueNone -> target.TextAlignment <- System.Windows.TextAlignment.Left
        | ValueNone, ValueNone -> ()

    static member inline ConstructTextBlock(?text: string,
                                            ?textAlignment: System.Windows.TextAlignment,
                                            ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                            ?margin: System.Windows.Thickness,
                                            ?verticalAlignment: System.Windows.VerticalAlignment,
                                            ?width: float) = 

        let attribBuilder = ViewBuilders.BuildTextBlock(0,
                               ?text=text,
                               ?textAlignment=textAlignment,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.TextBlock>(ViewBuilders.CreateTextBlock, (fun prevOpt curr target -> ViewBuilders.UpdateTextBlock(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a RangeBase in the view
    static member inline BuildRangeBase(attribCount: int,
                                        ?minimum: float,
                                        ?maximum: float,
                                        ?value: float,
                                        ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                        ?margin: System.Windows.Thickness,
                                        ?verticalAlignment: System.Windows.VerticalAlignment,
                                        ?width: float,
                                        ?valueChanged: float -> unit) = 

        let attribCount = match minimum with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match maximum with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match value with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match valueChanged with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match minimum with None -> () | Some v -> attribBuilder.Add(ViewAttributes.MinimumAttribKey, (v)) 
        match maximum with None -> () | Some v -> attribBuilder.Add(ViewAttributes.MaximumAttribKey, (v)) 
        match value with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ValueAttribKey, (v)) 
        match valueChanged with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ValueChangedAttribKey, (fun f -> System.Windows.RoutedPropertyChangedEventHandler<float>(fun _sender _args -> f _args.NewValue))(v)) 
        attribBuilder

    static member UpdateRangeBase (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Primitives.RangeBase) = 
        let mutable prevValueChangedOpt = ValueNone
        let mutable currValueChangedOpt = ValueNone
        let mutable prevMinimumOpt = ValueNone
        let mutable currMinimumOpt = ValueNone
        let mutable prevMaximumOpt = ValueNone
        let mutable currMaximumOpt = ValueNone
        let mutable prevValueOpt = ValueNone
        let mutable currValueOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ValueChangedAttribKey.KeyValue then 
                currValueChangedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedPropertyChangedEventHandler<System.Double>)
            if kvp.Key = ViewAttributes.MinimumAttribKey.KeyValue then 
                currMinimumOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.MaximumAttribKey.KeyValue then 
                currMaximumOpt <- ValueSome (kvp.Value :?> float)
            if kvp.Key = ViewAttributes.ValueAttribKey.KeyValue then 
                currValueOpt <- ValueSome (kvp.Value :?> float)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ValueChangedAttribKey.KeyValue then 
                    prevValueChangedOpt <- ValueSome (kvp.Value :?> System.Windows.RoutedPropertyChangedEventHandler<System.Double>)
                if kvp.Key = ViewAttributes.MinimumAttribKey.KeyValue then 
                    prevMinimumOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.MaximumAttribKey.KeyValue then 
                    prevMaximumOpt <- ValueSome (kvp.Value :?> float)
                if kvp.Key = ViewAttributes.ValueAttribKey.KeyValue then 
                    prevValueOpt <- ValueSome (kvp.Value :?> float)
        // Unsubscribe previous event handlers
        let shouldUpdateValueChanged = not ((identical prevValueChangedOpt currValueChangedOpt))
        if shouldUpdateValueChanged then
            match prevValueChangedOpt with
            | ValueSome prevValue -> target.ValueChanged.RemoveHandler(prevValue)
            | ValueNone -> ()
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevMinimumOpt, currMinimumOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Minimum <-  currValue
        | ValueSome _, ValueNone -> target.Minimum <- 0.
        | ValueNone, ValueNone -> ()
        match prevMaximumOpt, currMaximumOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Maximum <-  currValue
        | ValueSome _, ValueNone -> target.Maximum <- 1.
        | ValueNone, ValueNone -> ()
        match prevValueOpt, currValueOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Value <-  currValue
        | ValueSome _, ValueNone -> target.Value <- 0.
        | ValueNone, ValueNone -> ()
        // Subscribe new event handlers
        if shouldUpdateValueChanged then
            match currValueChangedOpt with
            | ValueSome currValue -> target.ValueChanged.AddHandler(currValue)
            | ValueNone -> ()

    /// Builds the attributes for a Slider in the view
    static member inline BuildSlider(attribCount: int,
                                     ?minimum: float,
                                     ?maximum: float,
                                     ?value: float,
                                     ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                     ?margin: System.Windows.Thickness,
                                     ?verticalAlignment: System.Windows.VerticalAlignment,
                                     ?width: float,
                                     ?valueChanged: float -> unit) = 
        let attribBuilder = ViewBuilders.BuildRangeBase(attribCount, ?minimum=minimum, ?maximum=maximum, ?value=value, ?horizontalAlignment=horizontalAlignment, ?margin=margin, 
                                                        ?verticalAlignment=verticalAlignment, ?width=width, ?valueChanged=valueChanged)
        attribBuilder

    static member CreateSlider () : System.Windows.Controls.Slider =
        new System.Windows.Controls.Slider()

    static member UpdateSlider (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Slider) = 
        ViewBuilders.UpdateRangeBase (prevOpt, curr, target)

    static member inline ConstructSlider(?minimum: float,
                                         ?maximum: float,
                                         ?value: float,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float,
                                         ?valueChanged: float -> unit) = 

        let attribBuilder = ViewBuilders.BuildSlider(0,
                               ?minimum=minimum,
                               ?maximum=maximum,
                               ?value=value,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width,
                               ?valueChanged=valueChanged)

        ViewElement.Create<System.Windows.Controls.Slider>(ViewBuilders.CreateSlider, (fun prevOpt curr target -> ViewBuilders.UpdateSlider(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a StackPanel in the view
    static member inline BuildStackPanel(attribCount: int,
                                         ?children: ViewElement list,
                                         ?orientation: System.Windows.Controls.Orientation,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float) = 

        let attribCount = match children with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match orientation with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match children with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ChildrenAttribKey, Array.ofList(v)) 
        match orientation with None -> () | Some v -> attribBuilder.Add(ViewAttributes.OrientationAttribKey, (v)) 
        attribBuilder

    static member CreateStackPanel () : System.Windows.Controls.StackPanel =
        new System.Windows.Controls.StackPanel()

    static member UpdateStackPanel (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.StackPanel) = 
        let mutable prevChildrenOpt = ValueNone
        let mutable currChildrenOpt = ValueNone
        let mutable prevOrientationOpt = ValueNone
        let mutable currOrientationOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                currChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
            if kvp.Key = ViewAttributes.OrientationAttribKey.KeyValue then 
                currOrientationOpt <- ValueSome (kvp.Value :?> System.Windows.Controls.Orientation)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                    prevChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
                if kvp.Key = ViewAttributes.OrientationAttribKey.KeyValue then 
                    prevOrientationOpt <- ValueSome (kvp.Value :?> System.Windows.Controls.Orientation)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        ViewUpdaters.updatePanelChildren prevChildrenOpt currChildrenOpt target
        match prevOrientationOpt, currOrientationOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Orientation <-  currValue
        | ValueSome _, ValueNone -> target.Orientation <- System.Windows.Controls.Orientation.Vertical
        | ValueNone, ValueNone -> ()

    static member inline ConstructStackPanel(?children: ViewElement list,
                                             ?orientation: System.Windows.Controls.Orientation,
                                             ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                             ?margin: System.Windows.Thickness,
                                             ?verticalAlignment: System.Windows.VerticalAlignment,
                                             ?width: float) = 

        let attribBuilder = ViewBuilders.BuildStackPanel(0,
                               ?children=children,
                               ?orientation=orientation,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.StackPanel>(ViewBuilders.CreateStackPanel, (fun prevOpt curr target -> ViewBuilders.UpdateStackPanel(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a Border in the view
    static member inline BuildBorder(attribCount: int,
                                     ?child: ViewElement,
                                     ?cornerRadius: System.Windows.CornerRadius,
                                     ?borderThickness: System.Windows.Thickness,
                                     ?padding: System.Windows.Thickness,
                                     ?borderBrush: System.Windows.Media.Brush,
                                     ?background: System.Windows.Media.Brush,
                                     ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                     ?margin: System.Windows.Thickness,
                                     ?verticalAlignment: System.Windows.VerticalAlignment,
                                     ?width: float) = 

        let attribCount = match child with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match cornerRadius with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match borderThickness with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match padding with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match borderBrush with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match background with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match child with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ChildAttribKey, (v)) 
        match cornerRadius with None -> () | Some v -> attribBuilder.Add(ViewAttributes.CornerRadiusAttribKey, (v)) 
        match borderThickness with None -> () | Some v -> attribBuilder.Add(ViewAttributes.BorderThicknessAttribKey, (v)) 
        match padding with None -> () | Some v -> attribBuilder.Add(ViewAttributes.PaddingAttribKey, (v)) 
        match borderBrush with None -> () | Some v -> attribBuilder.Add(ViewAttributes.BorderBrushAttribKey, (v)) 
        match background with None -> () | Some v -> attribBuilder.Add(ViewAttributes.BackgroundAttribKey, (v)) 
        attribBuilder

    static member CreateBorder () : System.Windows.Controls.Border =
        new System.Windows.Controls.Border()

    static member UpdateBorder (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.Border) = 
        let mutable prevChildOpt = ValueNone
        let mutable currChildOpt = ValueNone
        let mutable prevCornerRadiusOpt = ValueNone
        let mutable currCornerRadiusOpt = ValueNone
        let mutable prevBorderThicknessOpt = ValueNone
        let mutable currBorderThicknessOpt = ValueNone
        let mutable prevPaddingOpt = ValueNone
        let mutable currPaddingOpt = ValueNone
        let mutable prevBorderBrushOpt = ValueNone
        let mutable currBorderBrushOpt = ValueNone
        let mutable prevBackgroundOpt = ValueNone
        let mutable currBackgroundOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.ChildAttribKey.KeyValue then 
                currChildOpt <- ValueSome (kvp.Value :?> ViewElement)
            if kvp.Key = ViewAttributes.CornerRadiusAttribKey.KeyValue then 
                currCornerRadiusOpt <- ValueSome (kvp.Value :?> System.Windows.CornerRadius)
            if kvp.Key = ViewAttributes.BorderThicknessAttribKey.KeyValue then 
                currBorderThicknessOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
            if kvp.Key = ViewAttributes.PaddingAttribKey.KeyValue then 
                currPaddingOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
            if kvp.Key = ViewAttributes.BorderBrushAttribKey.KeyValue then 
                currBorderBrushOpt <- ValueSome (kvp.Value :?> System.Windows.Media.Brush)
            if kvp.Key = ViewAttributes.BackgroundAttribKey.KeyValue then 
                currBackgroundOpt <- ValueSome (kvp.Value :?> System.Windows.Media.Brush)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.ChildAttribKey.KeyValue then 
                    prevChildOpt <- ValueSome (kvp.Value :?> ViewElement)
                if kvp.Key = ViewAttributes.CornerRadiusAttribKey.KeyValue then 
                    prevCornerRadiusOpt <- ValueSome (kvp.Value :?> System.Windows.CornerRadius)
                if kvp.Key = ViewAttributes.BorderThicknessAttribKey.KeyValue then 
                    prevBorderThicknessOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
                if kvp.Key = ViewAttributes.PaddingAttribKey.KeyValue then 
                    prevPaddingOpt <- ValueSome (kvp.Value :?> System.Windows.Thickness)
                if kvp.Key = ViewAttributes.BorderBrushAttribKey.KeyValue then 
                    prevBorderBrushOpt <- ValueSome (kvp.Value :?> System.Windows.Media.Brush)
                if kvp.Key = ViewAttributes.BackgroundAttribKey.KeyValue then 
                    prevBackgroundOpt <- ValueSome (kvp.Value :?> System.Windows.Media.Brush)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevChildOpt, currChildOpt with
        // For structured objects, dependsOn on reference equality
        | ValueSome prevValue, ValueSome newValue when identical prevValue newValue -> ()
        | ValueSome prevValue, ValueSome newValue when canReuseView prevValue newValue ->
            newValue.UpdateIncremental(prevValue, target.Child)
        | _, ValueSome newValue ->
            target.Child <- (newValue.Create() :?> System.Windows.UIElement)
        | ValueSome _, ValueNone ->
            target.Child <- null
        | ValueNone, ValueNone -> ()
        match prevCornerRadiusOpt, currCornerRadiusOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.CornerRadius <-  currValue
        | ValueSome _, ValueNone -> target.CornerRadius <- System.Windows.CornerRadius(0.)
        | ValueNone, ValueNone -> ()
        match prevBorderThicknessOpt, currBorderThicknessOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.BorderThickness <-  currValue
        | ValueSome _, ValueNone -> target.BorderThickness <- System.Windows.Thickness(0.)
        | ValueNone, ValueNone -> ()
        match prevPaddingOpt, currPaddingOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Padding <-  currValue
        | ValueSome _, ValueNone -> target.Padding <- System.Windows.Thickness(0.)
        | ValueNone, ValueNone -> ()
        match prevBorderBrushOpt, currBorderBrushOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.BorderBrush <-  currValue
        | ValueSome _, ValueNone -> target.BorderBrush <- null
        | ValueNone, ValueNone -> ()
        match prevBackgroundOpt, currBackgroundOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Background <-  currValue
        | ValueSome _, ValueNone -> target.Background <- null
        | ValueNone, ValueNone -> ()

    static member inline ConstructBorder(?child: ViewElement,
                                         ?cornerRadius: System.Windows.CornerRadius,
                                         ?borderThickness: System.Windows.Thickness,
                                         ?padding: System.Windows.Thickness,
                                         ?borderBrush: System.Windows.Media.Brush,
                                         ?background: System.Windows.Media.Brush,
                                         ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                         ?margin: System.Windows.Thickness,
                                         ?verticalAlignment: System.Windows.VerticalAlignment,
                                         ?width: float) = 

        let attribBuilder = ViewBuilders.BuildBorder(0,
                               ?child=child,
                               ?cornerRadius=cornerRadius,
                               ?borderThickness=borderThickness,
                               ?padding=padding,
                               ?borderBrush=borderBrush,
                               ?background=background,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.Border>(ViewBuilders.CreateBorder, (fun prevOpt curr target -> ViewBuilders.UpdateBorder(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a RadioButton in the view
    static member inline BuildRadioButton(attribCount: int,
                                          ?groupName: System.String,
                                          ?content: System.Object,
                                          ?isChecked: bool option,
                                          ?command: unit -> unit,
                                          ?commandCanExecute: bool,
                                          ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                          ?margin: System.Windows.Thickness,
                                          ?verticalAlignment: System.Windows.VerticalAlignment,
                                          ?width: float,
                                          ?checked: unit -> unit,
                                          ?unchecked: unit -> unit) = 

        let attribCount = match groupName with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match content with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildToggleButton(attribCount, ?isChecked=isChecked, ?command=command, ?commandCanExecute=commandCanExecute, ?horizontalAlignment=horizontalAlignment, ?margin=margin, 
                                                           ?verticalAlignment=verticalAlignment, ?width=width, ?checked=checked, ?unchecked=unchecked)
        match groupName with None -> () | Some v -> attribBuilder.Add(ViewAttributes.GroupNameAttribKey, (v)) 
        match content with None -> () | Some v -> attribBuilder.Add(ViewAttributes.RadioButtonContentAttribKey, (v)) 
        attribBuilder

    static member CreateRadioButton () : System.Windows.Controls.RadioButton =
        new System.Windows.Controls.RadioButton()

    static member UpdateRadioButton (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.RadioButton) = 
        let mutable prevGroupNameOpt = ValueNone
        let mutable currGroupNameOpt = ValueNone
        let mutable prevRadioButtonContentOpt = ValueNone
        let mutable currRadioButtonContentOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.GroupNameAttribKey.KeyValue then 
                currGroupNameOpt <- ValueSome (kvp.Value :?> System.String)
            if kvp.Key = ViewAttributes.RadioButtonContentAttribKey.KeyValue then 
                currRadioButtonContentOpt <- ValueSome (kvp.Value :?> System.Object)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.GroupNameAttribKey.KeyValue then 
                    prevGroupNameOpt <- ValueSome (kvp.Value :?> System.String)
                if kvp.Key = ViewAttributes.RadioButtonContentAttribKey.KeyValue then 
                    prevRadioButtonContentOpt <- ValueSome (kvp.Value :?> System.Object)
        // Update inherited members
        ViewBuilders.UpdateToggleButton (prevOpt, curr, target)
        // Update properties
        match prevGroupNameOpt, currGroupNameOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.GroupName <-  currValue
        | ValueSome _, ValueNone -> target.GroupName <- System.String.Empty
        | ValueNone, ValueNone -> ()
        match prevRadioButtonContentOpt, currRadioButtonContentOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Content <-  currValue
        | ValueSome _, ValueNone -> target.Content <- null
        | ValueNone, ValueNone -> ()

    static member inline ConstructRadioButton(?groupName: System.String,
                                              ?content: System.Object,
                                              ?isChecked: bool option,
                                              ?command: unit -> unit,
                                              ?commandCanExecute: bool,
                                              ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                              ?margin: System.Windows.Thickness,
                                              ?verticalAlignment: System.Windows.VerticalAlignment,
                                              ?width: float,
                                              ?checked: unit -> unit,
                                              ?unchecked: unit -> unit) = 

        let attribBuilder = ViewBuilders.BuildRadioButton(0,
                               ?groupName=groupName,
                               ?content=content,
                               ?isChecked=isChecked,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width,
                               ?checked=checked,
                               ?unchecked=unchecked)

        ViewElement.Create<System.Windows.Controls.RadioButton>(ViewBuilders.CreateRadioButton, (fun prevOpt curr target -> ViewBuilders.UpdateRadioButton(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a DockPanel in the view
    static member inline BuildDockPanel(attribCount: int,
                                        ?lastChildFill: bool,
                                        ?children: ViewElement list,
                                        ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                        ?margin: System.Windows.Thickness,
                                        ?verticalAlignment: System.Windows.VerticalAlignment,
                                        ?width: float) = 

        let attribCount = match lastChildFill with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match children with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildFrameworkElement(attribCount, ?horizontalAlignment=horizontalAlignment, ?margin=margin, ?verticalAlignment=verticalAlignment, ?width=width)
        match lastChildFill with None -> () | Some v -> attribBuilder.Add(ViewAttributes.LastChildFillAttribKey, (v)) 
        match children with None -> () | Some v -> attribBuilder.Add(ViewAttributes.ChildrenAttribKey, Array.ofList(v)) 
        attribBuilder

    static member CreateDockPanel () : System.Windows.Controls.DockPanel =
        new System.Windows.Controls.DockPanel()

    static member UpdateDockPanel (prevOpt: ViewElement voption, curr: ViewElement, target: System.Windows.Controls.DockPanel) = 
        let mutable prevLastChildFillOpt = ValueNone
        let mutable currLastChildFillOpt = ValueNone
        let mutable prevChildrenOpt = ValueNone
        let mutable currChildrenOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.LastChildFillAttribKey.KeyValue then 
                currLastChildFillOpt <- ValueSome (kvp.Value :?> bool)
            if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                currChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.LastChildFillAttribKey.KeyValue then 
                    prevLastChildFillOpt <- ValueSome (kvp.Value :?> bool)
                if kvp.Key = ViewAttributes.ChildrenAttribKey.KeyValue then 
                    prevChildrenOpt <- ValueSome (kvp.Value :?> ViewElement array)
        // Update inherited members
        ViewBuilders.UpdateFrameworkElement (prevOpt, curr, target)
        // Update properties
        match prevLastChildFillOpt, currLastChildFillOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.LastChildFill <-  currValue
        | ValueSome _, ValueNone -> target.LastChildFill <- false
        | ValueNone, ValueNone -> ()
        ViewUpdaters.updatePanelChildren prevChildrenOpt currChildrenOpt target

    static member inline ConstructDockPanel(?lastChildFill: bool,
                                            ?children: ViewElement list,
                                            ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                            ?margin: System.Windows.Thickness,
                                            ?verticalAlignment: System.Windows.VerticalAlignment,
                                            ?width: float) = 

        let attribBuilder = ViewBuilders.BuildDockPanel(0,
                               ?lastChildFill=lastChildFill,
                               ?children=children,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

        ViewElement.Create<System.Windows.Controls.DockPanel>(ViewBuilders.CreateDockPanel, (fun prevOpt curr target -> ViewBuilders.UpdateDockPanel(prevOpt, curr, target)), attribBuilder)

/// Viewer that allows to read the properties of a ViewElement representing a UIElement
type UIElementViewer(element: ViewElement) =
    do if not ((typeof<System.Windows.UIElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.UIElement' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a FrameworkElement
type FrameworkElementViewer(element: ViewElement) =
    inherit UIElementViewer(element)
    do if not ((typeof<System.Windows.FrameworkElement>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.FrameworkElement' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the HorizontalAlignment member
    member this.HorizontalAlignment = element.GetAttributeKeyed(ViewAttributes.HorizontalAlignmentAttribKey)
    /// Get the value of the Margin member
    member this.Margin = element.GetAttributeKeyed(ViewAttributes.MarginAttribKey)
    /// Get the value of the VerticalAlignment member
    member this.VerticalAlignment = element.GetAttributeKeyed(ViewAttributes.VerticalAlignmentAttribKey)
    /// Get the value of the Width member
    member this.Width = element.GetAttributeKeyed(ViewAttributes.WidthAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a ContentControl
type ContentControlViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.ContentControl>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.ContentControl' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Content member
    member this.Content = element.GetAttributeKeyed(ViewAttributes.ContentControlContentAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Window
type WindowViewer(element: ViewElement) =
    inherit ContentControlViewer(element)
    do if not ((typeof<System.Windows.Window>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Window' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Title member
    member this.Title = element.GetAttributeKeyed(ViewAttributes.TitleAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a ButtonBase
type ButtonBaseViewer(element: ViewElement) =
    inherit ContentControlViewer(element)
    do if not ((typeof<System.Windows.Controls.Primitives.ButtonBase>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Primitives.ButtonBase' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Command member
    member this.Command = element.GetAttributeKeyed(ViewAttributes.CommandAttribKey)
    /// Get the value of the CommandCanExecute member
    member this.CommandCanExecute = element.GetAttributeKeyed(ViewAttributes.CommandCanExecuteAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Button
type ButtonViewer(element: ViewElement) =
    inherit ButtonBaseViewer(element)
    do if not ((typeof<System.Windows.Controls.Button>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Button' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Content member
    member this.Content = element.GetAttributeKeyed(ViewAttributes.ButtonContentAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a ToggleButton
type ToggleButtonViewer(element: ViewElement) =
    inherit ButtonBaseViewer(element)
    do if not ((typeof<System.Windows.Controls.Primitives.ToggleButton>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Primitives.ToggleButton' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the IsChecked member
    member this.IsChecked = element.GetAttributeKeyed(ViewAttributes.IsCheckedAttribKey)
    /// Get the value of the Checked member
    member this.Checked = element.GetAttributeKeyed(ViewAttributes.CheckedAttribKey)
    /// Get the value of the Unchecked member
    member this.Unchecked = element.GetAttributeKeyed(ViewAttributes.UncheckedAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a CheckBox
type CheckBoxViewer(element: ViewElement) =
    inherit ToggleButtonViewer(element)
    do if not ((typeof<System.Windows.Controls.CheckBox>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.CheckBox' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a TextBlock
type TextBlockViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.TextBlock>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.TextBlock' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Text member
    member this.Text = element.GetAttributeKeyed(ViewAttributes.TextAttribKey)
    /// Get the value of the TextAlignment member
    member this.TextAlignment = element.GetAttributeKeyed(ViewAttributes.TextAlignmentAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a RangeBase
type RangeBaseViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.Primitives.RangeBase>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Primitives.RangeBase' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Minimum member
    member this.Minimum = element.GetAttributeKeyed(ViewAttributes.MinimumAttribKey)
    /// Get the value of the Maximum member
    member this.Maximum = element.GetAttributeKeyed(ViewAttributes.MaximumAttribKey)
    /// Get the value of the Value member
    member this.Value = element.GetAttributeKeyed(ViewAttributes.ValueAttribKey)
    /// Get the value of the ValueChanged member
    member this.ValueChanged = element.GetAttributeKeyed(ViewAttributes.ValueChangedAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Slider
type SliderViewer(element: ViewElement) =
    inherit RangeBaseViewer(element)
    do if not ((typeof<System.Windows.Controls.Slider>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Slider' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a StackPanel
type StackPanelViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.StackPanel>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.StackPanel' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Children member
    member this.Children = element.GetAttributeKeyed(ViewAttributes.ChildrenAttribKey)
    /// Get the value of the Orientation member
    member this.Orientation = element.GetAttributeKeyed(ViewAttributes.OrientationAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a Border
type BorderViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.Border>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.Border' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Child member
    member this.Child = element.GetAttributeKeyed(ViewAttributes.ChildAttribKey)
    /// Get the value of the CornerRadius member
    member this.CornerRadius = element.GetAttributeKeyed(ViewAttributes.CornerRadiusAttribKey)
    /// Get the value of the BorderThickness member
    member this.BorderThickness = element.GetAttributeKeyed(ViewAttributes.BorderThicknessAttribKey)
    /// Get the value of the Padding member
    member this.Padding = element.GetAttributeKeyed(ViewAttributes.PaddingAttribKey)
    /// Get the value of the BorderBrush member
    member this.BorderBrush = element.GetAttributeKeyed(ViewAttributes.BorderBrushAttribKey)
    /// Get the value of the Background member
    member this.Background = element.GetAttributeKeyed(ViewAttributes.BackgroundAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a RadioButton
type RadioButtonViewer(element: ViewElement) =
    inherit ToggleButtonViewer(element)
    do if not ((typeof<System.Windows.Controls.RadioButton>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.RadioButton' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the GroupName member
    member this.GroupName = element.GetAttributeKeyed(ViewAttributes.GroupNameAttribKey)
    /// Get the value of the Content member
    member this.Content = element.GetAttributeKeyed(ViewAttributes.RadioButtonContentAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a DockPanel
type DockPanelViewer(element: ViewElement) =
    inherit FrameworkElementViewer(element)
    do if not ((typeof<System.Windows.Controls.DockPanel>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'System.Windows.Controls.DockPanel' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the LastChildFill member
    member this.LastChildFill = element.GetAttributeKeyed(ViewAttributes.LastChildFillAttribKey)
    /// Get the value of the Children member
    member this.Children = element.GetAttributeKeyed(ViewAttributes.ChildrenAttribKey)

[<AbstractClass; Sealed>]
type View private () =
    /// Describes a ContentControl in the view
    static member inline ContentControl(?content: ViewElement,
                                        ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                        ?margin: System.Windows.Thickness,
                                        ?verticalAlignment: System.Windows.VerticalAlignment,
                                        ?width: float) =

        ViewBuilders.ConstructContentControl(?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Window in the view
    static member inline Window(?content: ViewElement,
                                ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                ?margin: System.Windows.Thickness,
                                ?title: string,
                                ?verticalAlignment: System.Windows.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructWindow(?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?title=title,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Button in the view
    static member inline Button(?command: unit -> unit,
                                ?commandCanExecute: bool,
                                ?content: string,
                                ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                ?margin: System.Windows.Thickness,
                                ?verticalAlignment: System.Windows.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructButton(?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a CheckBox in the view
    static member inline CheckBox(?checked: unit -> unit,
                                  ?command: unit -> unit,
                                  ?commandCanExecute: bool,
                                  ?content: ViewElement,
                                  ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                  ?isChecked: bool option,
                                  ?margin: System.Windows.Thickness,
                                  ?unchecked: unit -> unit,
                                  ?verticalAlignment: System.Windows.VerticalAlignment,
                                  ?width: float) =

        ViewBuilders.ConstructCheckBox(?checked=checked,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?horizontalAlignment=horizontalAlignment,
                               ?isChecked=isChecked,
                               ?margin=margin,
                               ?unchecked=unchecked,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a TextBlock in the view
    static member inline TextBlock(?text: string,
                                   ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                   ?margin: System.Windows.Thickness,
                                   ?textAlignment: System.Windows.TextAlignment,
                                   ?verticalAlignment: System.Windows.VerticalAlignment,
                                   ?width: float) =

        ViewBuilders.ConstructTextBlock(?text=text,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?textAlignment=textAlignment,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Slider in the view
    static member inline Slider(?horizontalAlignment: System.Windows.HorizontalAlignment,
                                ?margin: System.Windows.Thickness,
                                ?maximum: float,
                                ?minimum: float,
                                ?value: float,
                                ?valueChanged: float -> unit,
                                ?verticalAlignment: System.Windows.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructSlider(?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?maximum=maximum,
                               ?minimum=minimum,
                               ?value=value,
                               ?valueChanged=valueChanged,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a StackPanel in the view
    static member inline StackPanel(?children: ViewElement list,
                                    ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                    ?margin: System.Windows.Thickness,
                                    ?orientation: System.Windows.Controls.Orientation,
                                    ?verticalAlignment: System.Windows.VerticalAlignment,
                                    ?width: float) =

        ViewBuilders.ConstructStackPanel(?children=children,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?orientation=orientation,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a Border in the view
    static member inline Border(?background: System.Windows.Media.Brush,
                                ?borderBrush: System.Windows.Media.Brush,
                                ?borderThickness: System.Windows.Thickness,
                                ?child: ViewElement,
                                ?cornerRadius: System.Windows.CornerRadius,
                                ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                ?margin: System.Windows.Thickness,
                                ?padding: System.Windows.Thickness,
                                ?verticalAlignment: System.Windows.VerticalAlignment,
                                ?width: float) =

        ViewBuilders.ConstructBorder(?background=background,
                               ?borderBrush=borderBrush,
                               ?borderThickness=borderThickness,
                               ?child=child,
                               ?cornerRadius=cornerRadius,
                               ?horizontalAlignment=horizontalAlignment,
                               ?margin=margin,
                               ?padding=padding,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a RadioButton in the view
    static member inline RadioButton(?checked: unit -> unit,
                                     ?command: unit -> unit,
                                     ?commandCanExecute: bool,
                                     ?content: System.Object,
                                     ?groupName: System.String,
                                     ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                     ?isChecked: bool option,
                                     ?margin: System.Windows.Thickness,
                                     ?unchecked: unit -> unit,
                                     ?verticalAlignment: System.Windows.VerticalAlignment,
                                     ?width: float) =

        ViewBuilders.ConstructRadioButton(?checked=checked,
                               ?command=command,
                               ?commandCanExecute=commandCanExecute,
                               ?content=content,
                               ?groupName=groupName,
                               ?horizontalAlignment=horizontalAlignment,
                               ?isChecked=isChecked,
                               ?margin=margin,
                               ?unchecked=unchecked,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)

    /// Describes a DockPanel in the view
    static member inline DockPanel(?children: ViewElement list,
                                   ?horizontalAlignment: System.Windows.HorizontalAlignment,
                                   ?lastChildFill: bool,
                                   ?margin: System.Windows.Thickness,
                                   ?verticalAlignment: System.Windows.VerticalAlignment,
                                   ?width: float) =

        ViewBuilders.ConstructDockPanel(?children=children,
                               ?horizontalAlignment=horizontalAlignment,
                               ?lastChildFill=lastChildFill,
                               ?margin=margin,
                               ?verticalAlignment=verticalAlignment,
                               ?width=width)


[<AutoOpen>]
module ViewElementExtensions = 

    type ViewElement with

        /// Adjusts the HorizontalAlignment property in the visual element
        member x.HorizontalAlignment(value: System.Windows.HorizontalAlignment) = x.WithAttribute(ViewAttributes.HorizontalAlignmentAttribKey, (value))

        /// Adjusts the Margin property in the visual element
        member x.Margin(value: System.Windows.Thickness) = x.WithAttribute(ViewAttributes.MarginAttribKey, (value))

        /// Adjusts the VerticalAlignment property in the visual element
        member x.VerticalAlignment(value: System.Windows.VerticalAlignment) = x.WithAttribute(ViewAttributes.VerticalAlignmentAttribKey, (value))

        /// Adjusts the Width property in the visual element
        member x.Width(value: float) = x.WithAttribute(ViewAttributes.WidthAttribKey, (value))

        /// Adjusts the ContentControlContent property in the visual element
        member x.ContentControlContent(value: ViewElement) = x.WithAttribute(ViewAttributes.ContentControlContentAttribKey, (value))

        /// Adjusts the Title property in the visual element
        member x.Title(value: string) = x.WithAttribute(ViewAttributes.TitleAttribKey, (value))

        /// Adjusts the Command property in the visual element
        member x.Command(value: unit -> unit) = x.WithAttribute(ViewAttributes.CommandAttribKey, (value))

        /// Adjusts the CommandCanExecute property in the visual element
        member x.CommandCanExecute(value: bool) = x.WithAttribute(ViewAttributes.CommandCanExecuteAttribKey, (value))

        /// Adjusts the ButtonContent property in the visual element
        member x.ButtonContent(value: string) = x.WithAttribute(ViewAttributes.ButtonContentAttribKey, (value))

        /// Adjusts the Checked property in the visual element
        member x.Checked(value: unit -> unit) = x.WithAttribute(ViewAttributes.CheckedAttribKey, (fun f -> System.Windows.RoutedEventHandler(fun _sender _args -> f()))(value))

        /// Adjusts the Unchecked property in the visual element
        member x.Unchecked(value: unit -> unit) = x.WithAttribute(ViewAttributes.UncheckedAttribKey, (fun f -> System.Windows.RoutedEventHandler(fun _sender _args -> f()))(value))

        /// Adjusts the IsChecked property in the visual element
        member x.IsChecked(value: bool option) = x.WithAttribute(ViewAttributes.IsCheckedAttribKey, (value))

        /// Adjusts the Text property in the visual element
        member x.Text(value: string) = x.WithAttribute(ViewAttributes.TextAttribKey, (value))

        /// Adjusts the TextAlignment property in the visual element
        member x.TextAlignment(value: System.Windows.TextAlignment) = x.WithAttribute(ViewAttributes.TextAlignmentAttribKey, (value))

        /// Adjusts the ValueChanged property in the visual element
        member x.ValueChanged(value: float -> unit) = x.WithAttribute(ViewAttributes.ValueChangedAttribKey, (fun f -> System.Windows.RoutedPropertyChangedEventHandler<float>(fun _sender _args -> f _args.NewValue))(value))

        /// Adjusts the Minimum property in the visual element
        member x.Minimum(value: float) = x.WithAttribute(ViewAttributes.MinimumAttribKey, (value))

        /// Adjusts the Maximum property in the visual element
        member x.Maximum(value: float) = x.WithAttribute(ViewAttributes.MaximumAttribKey, (value))

        /// Adjusts the Value property in the visual element
        member x.Value(value: float) = x.WithAttribute(ViewAttributes.ValueAttribKey, (value))

        /// Adjusts the Children property in the visual element
        member x.Children(value: ViewElement list) = x.WithAttribute(ViewAttributes.ChildrenAttribKey, Array.ofList(value))

        /// Adjusts the Orientation property in the visual element
        member x.Orientation(value: System.Windows.Controls.Orientation) = x.WithAttribute(ViewAttributes.OrientationAttribKey, (value))

        /// Adjusts the Child property in the visual element
        member x.Child(value: ViewElement) = x.WithAttribute(ViewAttributes.ChildAttribKey, (value))

        /// Adjusts the CornerRadius property in the visual element
        member x.CornerRadius(value: System.Windows.CornerRadius) = x.WithAttribute(ViewAttributes.CornerRadiusAttribKey, (value))

        /// Adjusts the BorderThickness property in the visual element
        member x.BorderThickness(value: System.Windows.Thickness) = x.WithAttribute(ViewAttributes.BorderThicknessAttribKey, (value))

        /// Adjusts the Padding property in the visual element
        member x.Padding(value: System.Windows.Thickness) = x.WithAttribute(ViewAttributes.PaddingAttribKey, (value))

        /// Adjusts the BorderBrush property in the visual element
        member x.BorderBrush(value: System.Windows.Media.Brush) = x.WithAttribute(ViewAttributes.BorderBrushAttribKey, (value))

        /// Adjusts the Background property in the visual element
        member x.Background(value: System.Windows.Media.Brush) = x.WithAttribute(ViewAttributes.BackgroundAttribKey, (value))

        /// Adjusts the GroupName property in the visual element
        member x.GroupName(value: System.String) = x.WithAttribute(ViewAttributes.GroupNameAttribKey, (value))

        /// Adjusts the RadioButtonContent property in the visual element
        member x.RadioButtonContent(value: System.Object) = x.WithAttribute(ViewAttributes.RadioButtonContentAttribKey, (value))

        /// Adjusts the LastChildFill property in the visual element
        member x.LastChildFill(value: bool) = x.WithAttribute(ViewAttributes.LastChildFillAttribKey, (value))

        member inline x.With(?horizontalAlignment: System.Windows.HorizontalAlignment, ?margin: System.Windows.Thickness, ?verticalAlignment: System.Windows.VerticalAlignment, ?width: float, ?contentControlContent: ViewElement, 
                             ?title: string, ?command: unit -> unit, ?commandCanExecute: bool, ?buttonContent: string, ?checked: unit -> unit, 
                             ?unchecked: unit -> unit, ?isChecked: bool option, ?text: string, ?textAlignment: System.Windows.TextAlignment, ?valueChanged: float -> unit, 
                             ?minimum: float, ?maximum: float, ?value: float, ?children: ViewElement list, ?orientation: System.Windows.Controls.Orientation, 
                             ?child: ViewElement, ?cornerRadius: System.Windows.CornerRadius, ?borderThickness: System.Windows.Thickness, ?padding: System.Windows.Thickness, ?borderBrush: System.Windows.Media.Brush, 
                             ?background: System.Windows.Media.Brush, ?groupName: System.String, ?radioButtonContent: System.Object, ?lastChildFill: bool) =
            let x = match horizontalAlignment with None -> x | Some opt -> x.HorizontalAlignment(opt)
            let x = match margin with None -> x | Some opt -> x.Margin(opt)
            let x = match verticalAlignment with None -> x | Some opt -> x.VerticalAlignment(opt)
            let x = match width with None -> x | Some opt -> x.Width(opt)
            let x = match contentControlContent with None -> x | Some opt -> x.ContentControlContent(opt)
            let x = match title with None -> x | Some opt -> x.Title(opt)
            let x = match command with None -> x | Some opt -> x.Command(opt)
            let x = match commandCanExecute with None -> x | Some opt -> x.CommandCanExecute(opt)
            let x = match buttonContent with None -> x | Some opt -> x.ButtonContent(opt)
            let x = match checked with None -> x | Some opt -> x.Checked(opt)
            let x = match unchecked with None -> x | Some opt -> x.Unchecked(opt)
            let x = match isChecked with None -> x | Some opt -> x.IsChecked(opt)
            let x = match text with None -> x | Some opt -> x.Text(opt)
            let x = match textAlignment with None -> x | Some opt -> x.TextAlignment(opt)
            let x = match valueChanged with None -> x | Some opt -> x.ValueChanged(opt)
            let x = match minimum with None -> x | Some opt -> x.Minimum(opt)
            let x = match maximum with None -> x | Some opt -> x.Maximum(opt)
            let x = match value with None -> x | Some opt -> x.Value(opt)
            let x = match children with None -> x | Some opt -> x.Children(opt)
            let x = match orientation with None -> x | Some opt -> x.Orientation(opt)
            let x = match child with None -> x | Some opt -> x.Child(opt)
            let x = match cornerRadius with None -> x | Some opt -> x.CornerRadius(opt)
            let x = match borderThickness with None -> x | Some opt -> x.BorderThickness(opt)
            let x = match padding with None -> x | Some opt -> x.Padding(opt)
            let x = match borderBrush with None -> x | Some opt -> x.BorderBrush(opt)
            let x = match background with None -> x | Some opt -> x.Background(opt)
            let x = match groupName with None -> x | Some opt -> x.GroupName(opt)
            let x = match radioButtonContent with None -> x | Some opt -> x.RadioButtonContent(opt)
            let x = match lastChildFill with None -> x | Some opt -> x.LastChildFill(opt)
            x

    /// Adjusts the HorizontalAlignment property in the visual element
    let horizontalAlignment (value: System.Windows.HorizontalAlignment) (x: ViewElement) = x.HorizontalAlignment(value)
    /// Adjusts the Margin property in the visual element
    let margin (value: System.Windows.Thickness) (x: ViewElement) = x.Margin(value)
    /// Adjusts the VerticalAlignment property in the visual element
    let verticalAlignment (value: System.Windows.VerticalAlignment) (x: ViewElement) = x.VerticalAlignment(value)
    /// Adjusts the Width property in the visual element
    let width (value: float) (x: ViewElement) = x.Width(value)
    /// Adjusts the ContentControlContent property in the visual element
    let contentControlContent (value: ViewElement) (x: ViewElement) = x.ContentControlContent(value)
    /// Adjusts the Title property in the visual element
    let title (value: string) (x: ViewElement) = x.Title(value)
    /// Adjusts the Command property in the visual element
    let command (value: unit -> unit) (x: ViewElement) = x.Command(value)
    /// Adjusts the CommandCanExecute property in the visual element
    let commandCanExecute (value: bool) (x: ViewElement) = x.CommandCanExecute(value)
    /// Adjusts the ButtonContent property in the visual element
    let buttonContent (value: string) (x: ViewElement) = x.ButtonContent(value)
    /// Adjusts the Checked property in the visual element
    let checked (value: unit -> unit) (x: ViewElement) = x.Checked(value)
    /// Adjusts the Unchecked property in the visual element
    let unchecked (value: unit -> unit) (x: ViewElement) = x.Unchecked(value)
    /// Adjusts the IsChecked property in the visual element
    let isChecked (value: bool option) (x: ViewElement) = x.IsChecked(value)
    /// Adjusts the Text property in the visual element
    let text (value: string) (x: ViewElement) = x.Text(value)
    /// Adjusts the TextAlignment property in the visual element
    let textAlignment (value: System.Windows.TextAlignment) (x: ViewElement) = x.TextAlignment(value)
    /// Adjusts the ValueChanged property in the visual element
    let valueChanged (value: float -> unit) (x: ViewElement) = x.ValueChanged(value)
    /// Adjusts the Minimum property in the visual element
    let minimum (value: float) (x: ViewElement) = x.Minimum(value)
    /// Adjusts the Maximum property in the visual element
    let maximum (value: float) (x: ViewElement) = x.Maximum(value)
    /// Adjusts the Value property in the visual element
    let value (value: float) (x: ViewElement) = x.Value(value)
    /// Adjusts the Children property in the visual element
    let children (value: ViewElement list) (x: ViewElement) = x.Children(value)
    /// Adjusts the Orientation property in the visual element
    let orientation (value: System.Windows.Controls.Orientation) (x: ViewElement) = x.Orientation(value)
    /// Adjusts the Child property in the visual element
    let child (value: ViewElement) (x: ViewElement) = x.Child(value)
    /// Adjusts the CornerRadius property in the visual element
    let cornerRadius (value: System.Windows.CornerRadius) (x: ViewElement) = x.CornerRadius(value)
    /// Adjusts the BorderThickness property in the visual element
    let borderThickness (value: System.Windows.Thickness) (x: ViewElement) = x.BorderThickness(value)
    /// Adjusts the Padding property in the visual element
    let padding (value: System.Windows.Thickness) (x: ViewElement) = x.Padding(value)
    /// Adjusts the BorderBrush property in the visual element
    let borderBrush (value: System.Windows.Media.Brush) (x: ViewElement) = x.BorderBrush(value)
    /// Adjusts the Background property in the visual element
    let background (value: System.Windows.Media.Brush) (x: ViewElement) = x.Background(value)
    /// Adjusts the GroupName property in the visual element
    let groupName (value: System.String) (x: ViewElement) = x.GroupName(value)
    /// Adjusts the RadioButtonContent property in the visual element
    let radioButtonContent (value: System.Object) (x: ViewElement) = x.RadioButtonContent(value)
    /// Adjusts the LastChildFill property in the visual element
    let lastChildFill (value: bool) (x: ViewElement) = x.LastChildFill(value)
