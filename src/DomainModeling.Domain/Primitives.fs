module DomainModeling.Domain.Primitives

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

