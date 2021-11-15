namespace OrderTaking.Domain

type Undefined = exn

// Product code related
type WidgetCode = WidgetCode of string // constraint: starting with W then 4 digits
type GizmoCode = GizmoCode of string // constraint: starting with G then 4 digits

type ProductCode =
  | Widget of WidgetCode
  | Gizmo of GizmoCode

// OrderQuantity related
type UnitQuantity = UnitQuantity of int
type KilogramQuantity = KilogramQuantity of decimal

type OrderQuantity =
  | Unit of UnitQuantity
  | Kilos of KilogramQuantity

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
    OrderLines: OrderLine list
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
    ShippingAddress: Undefined (* other data *)  }

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
