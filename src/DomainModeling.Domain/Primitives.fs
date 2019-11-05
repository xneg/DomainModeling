namespace DomainModeling.Domain.Primitives

open System;

type String50 = private String50 of string

type WidgetCode = WidgetCode of string
type GizmoCode = GizmoCode of string
type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode
    
type UnitQuantity = UnitQuantity of int
type KilogramQuantity = KilogramQuantity of decimal
type OrderQuantity =
    | Unit of UnitQuantity
    | Kilos of KilogramQuantity

type OrderId = private OrderId of string
type EmailAddress = private EmailAddress of string

type ZipCode = private ZipCode of string

module OrderId =
    let create str =
        if String.IsNullOrEmpty(str) then
            failwith "OrderId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderId must not be more then 50 chars"
        else
            OrderId str
    
    let value (OrderId str) = str

module ConstrainedType =

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null, empty, or length > maxLen
//    let createString fieldName ctor maxLen str = 
//        if String.IsNullOrEmpty(str) then
//            let msg = sprintf "%s must not be null or empty" fieldName 
//            Error msg
//        elif str.Length > maxLen then
//            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
//            Error msg 
//        else
//            Ok (ctor str)
            
    let createString fieldName ctor maxLen str = 
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s must not be null or empty" fieldName 
            failwith msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            failwith msg
        else
            ctor str

    /// Create a optional constrained string using the constructor provided
    /// Return None if input is null, empty. 
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    let createStringOption fieldName ctor maxLen str = 
        if String.IsNullOrEmpty(str) then
            Ok None
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            Error msg 
        else
            Ok (ctor str |> Some)

    /// Create a constrained integer using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createInt fieldName ctor minVal maxVal i = 
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %i" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %i" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained decimal using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createDecimal fieldName ctor minVal maxVal i = 
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %M" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null. empty, or does not match the regex pattern
//    let createLike fieldName  ctor pattern str = 
//        if String.IsNullOrEmpty(str) then
//            let msg = sprintf "%s: Must not be null or empty" fieldName 
//            Error msg
//        elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
//            Ok (ctor str)
//        else
//            let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
//            Error msg
            
    let createLike fieldName  ctor pattern str = 
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s: Must not be null or empty" fieldName 
            failwith msg
        elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
            ctor str
        else
            let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
            failwith msg
    
module String50 =
    let value (String50 str) = str

    let create fieldName str = ConstrainedType.createString fieldName String50 50 str

    let createOption fieldName str = ConstrainedType.createStringOption fieldName String50 50 str
    
module EmailAddress =
    let value (EmailAddress str) = str

    let create fieldName str = 
        let pattern = ".+@.+" // anything separated by an "@"
        ConstrainedType.createLike fieldName EmailAddress pattern str
        
module ZipCode =

    /// Return the string value inside a ZipCode
    let value (ZipCode str) = str

    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    let create fieldName str = 
        let pattern = "\d{5}"
        ConstrainedType.createLike fieldName ZipCode pattern str
