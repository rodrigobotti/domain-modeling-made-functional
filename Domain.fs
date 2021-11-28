namespace OrderTaking.Domain

open FSharpPlus.Data

type Undefined = exn

// Product code related
type WidgetCode = WidgetCode of string // constraint: starting with W then 4 digits
type GizmoCode = GizmoCode of string // constraint: starting with G then 4 digits

type ProductCode =
  | Widget of WidgetCode
  | Gizmo of GizmoCode

// OrderQuantity related
type UnitQuantity = private UnitQuantity of int

module UnitQuantity =
  /// smart constructor
  let create qty =
    if qty < 1 then
      Error "UnityQuantity cannot be negative"
    else if qty > 1000 then
      Error "UnityQuantity cannot be more than 1000"
    else
      Ok(UnitQuantity qty)

  let value (UnitQuantity qty) = qty

// units of measure have no runtime performance hit; used only at compile time
[<Measure>]
type kg

type KilogramQuantity = KilogramQuantity of decimal<kg>

type OrderQuantity =
  | Unit of UnitQuantity
  | Kilos of KilogramQuantity

type UnvalidatedAddress = Undefined
type ValidatedAddress = private ValidatedAddress of Undefined

type AddressValidationService = UnvalidatedAddress -> ValidatedAddress option

type OrderId = Undefined
type OrderLineId = Undefined
type CustomerId = Undefined

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type Price = Undefined
type BillingAmount = Undefined

type Order =
  { Id: OrderId // id for entity
    CustomerId: CustomerId // customer reference -> order aggregate should not have a Customer record inside: consistency boundaries
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: NonEmptyList<OrderLine>
    AmountToBill: BillingAmount }

and OrderLine =
  { Id: OrderLineId // id for entity
    OrderId: OrderId
    ProductCode: ProductCode
    OrderQuantity: OrderQuantity
    Proce: Price }

// Place Order workflow

// made of primitive types: "as is" from form
type UnvalidatedOrder =
  { OrderId: string
    CustomerInfo: Undefined
    ShippingAddress: UnvalidatedAddress (* other data *)  }

// output: single record with all output events
type PlaceOrderEvents =
  { AcknowledgementSent: Undefined
    OrderPlaced: Undefined
    BillableOrderPlaced: Undefined }

type PlaceOrderError = ValidationError of ValidationError list
// | ... other errors
and ValidationError =
  { FieldName: string
    ErrorDescription: string }

/// The "Place Order" process
type PlaceOrder = UnvalidatedOrder -> Result<PlaceOrderEvents, PlaceOrderError>
