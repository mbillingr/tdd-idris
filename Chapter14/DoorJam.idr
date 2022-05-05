
data DoorResult = OK | Jammed

data DoorState = DoorOpen | DoorClosed

data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
     Open : DoorCmd DoorResult DoorClosed (\res => case res of 
                                                        OK => DoorOpen
                                                        Jammed => DoorClosed)
     Close : DoorCmd () DoorOpen (const DoorClosed)
     RingBell : DoorCmd () DoorClosed (const DoorClosed)

     Display : String -> DoorCmd () state (const state)

     Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn
     (>>=) : DoorCmd a st1 st2_fn ->
             ((res : a) -> DoorCmd b (st2_fn res) st3_fn) ->
             DoorCmd b st1 st3_fn

(>>) : DoorCmd () st1 st2_fn -> Inf (DoorCmd b (st2_fn ()) st3_fn) -> DoorCmd b st1 st3_fn
ma >> mb = ma >>= \() => mb


doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do RingBell
              jam <- Open
              case jam of
                   OK => do Display "Glad To Be Of Service"
                            Close
                   Jammed => Display "Door Jammed"

doorProgSimpler : DoorCmd () DoorClosed (const DoorClosed)
doorProgSimpler = do RingBell
                     OK <- Open | Jammed => Display "Door Jammed"
                     Display "Glad To Be Of Service"
                     Close