module Vish.Layout where

import Control.Lens
import Linear

data Layout =
  Layout
    HAlignment -- ^ Horizontal Alignment
    VAlignment -- ^ Vertical Alignment
    Sizing    -- ^ How the layout fits

data HAlignment =
    AlignLeft
  | AlignCenterH
  | AlignRight

data VAlignment =
    AlignTop
  | AlignCenterV
  | AlignBottom

data Sizing = None | Fit | Fill | Stretch | StretchX | StretchY

toSides :: V2 Double -- ^ Position
        -> V2 Double -- ^ Size
        -> V4 Double -- ^ x1 x2 y1 y2
toSides (V2 x y) (V2 w h) =
  V4 x (x+w) y (y+h)

toPosSize :: V4 Double -- x1 x2 y1 y2
          -> (V2 Double, V2 Double) -- position and size
toPosSize (V4 x1 x2 y1 y2) =
  (V2 x1 y1, V2 (x2-x1) (y2-y1))


layout :: Layout   -- ^ Properties for the layout
       -> V2 Double -- ^ Cell position
       -> V2 Double -- ^ Cell size
       -> V2 Double -- ^ Element size
       -> (V2 Double, V2 Double) -- ^ Elem position and size in cell with layout.
layout (Layout hA vA s) cP cS eS  =
  let c = toSides cP cS
      e = toSides zero eS
  in toPosSize . size s c . align hA vA c $ e

align :: HAlignment -- ^ Horizontal alignment
      -> VAlignment -- ^ Vertical alignment
      -> V4 Double  -- ^ Cell coordinates
      -> V4 Double  -- ^ Element coordinatess
      -> V4 Double  -- ^ Aligned elemnet coordinates
align hA vA c =
  alignV vA c . alignH hA c

alignH :: HAlignment -> V4 Double -> V4 Double -> V4 Double
alignH AlignLeft    = alignLeft
alignH AlignCenterH = alignCenterH
alignH AlignRight   = alignRight

alignV :: VAlignment -> V4 Double -> V4 Double -> V4 Double
alignV AlignTop     = alignTop
alignV AlignCenterV = alignCenterV
alignV AlignBottom  = alignBottom

alignLeft :: V4 Double -- ^ Cell coords
          -> V4 Double -- ^ Element coords
          -> V4 Double -- ^ Aligned eLement coords
alignLeft c e =
  e & (_x .~ eX1')
    . (_y .~ eX2')
  where
    eW  = (e ^. _y) - (e ^. _x)
    eX1' = c^._x
    eX2' = eX1' + eW


alignCenterH :: V4 Double -- ^ Cell coords
             -> V4 Double -- ^ Element coords
             -> V4 Double -- ^ Aligned eLement coords
alignCenterH c e =
  e & (_x .~ eX1')
    . (_y .~ eX2')
  where
    cW = (c ^. _y) - (c ^. _x)
    eW = (e ^. _y) - (e ^. _x)
    m  = (cW - eW) / 2
    eX1' = (c^._x) + m
    eX2' = eX1' + eW


alignRight :: V4 Double -- ^ Cell x1, x2
           -> V4 Double -- ^ Element x1, x2
           -> V4 Double -- ^ ELement x1', x2'
alignRight c e =
  e & (_x .~ eX1')
    . (_y .~ eX2')
  where
    eW   = (e ^. _y) - (e ^. _x)
    eX1' = eX2' - eW
    eX2' = c^._y


alignTop :: V4 Double -- ^ Cell coords
         -> V4 Double -- ^ Element coords
         -> V4 Double -- ^ Aligned eLement coords
alignTop c e =
  e & (_z .~ eY1')
    . (_w .~ eY2')
  where
    eH  = (e ^. _w) - (e ^. _z)
    eY1' = c^._z
    eY2' = eY1' + eH


alignCenterV :: V4 Double -- ^ Cell coords
             -> V4 Double -- ^ Element coords
             -> V4 Double -- ^ Aligned eLement coords
alignCenterV c e =
  e & (_z .~ eY1')
    . (_w .~ eY2')
  where
    cH = (c^._w) - (c^._z)
    eH = (e^._w) - (e^._z)
    m  = (cH - eH) / 2
    eY1' = (c^._z) + m
    eY2' = eY1' + eH


alignBottom :: V4 Double -- ^ Cell x1, x2
            -> V4 Double -- ^ Element x1, x2
            -> V4 Double -- ^ ELement x1', x2'
alignBottom c e =
  e & (_z .~ eY1')
    . (_w .~ eY2')
  where
    eH   = (e ^. _w) - (e ^. _z)
    eY1' = eY2' - eH
    eY2' = c^._w


size :: Sizing   -- ^ Sizing to adjust element to
     -> V4 Double -- ^ Cell coordinates
     -> V4 Double -- ^ Element coordinates
     -> V4 Double -- ^ Element coordinates after
size None     _ = id
size Fit      c = sizeFit c
size Fill     c = sizeFill c
size Stretch  c = sizeStretchX c . sizeStretchY c
size StretchX c = sizeStretchX c
size StretchY c = sizeStretchY c


sizeFit :: V4 Double -- ^ Cell coordinates
        -> V4 Double -- ^ Element coordinates
        -> V4 Double -- ^ Element coordinates after
sizeFit =
  scaleBy min

sizeFill :: V4 Double -- ^ Cell coordinates
         -> V4 Double -- ^ Element coordinates
         -> V4 Double -- ^ Element coordinates after
sizeFill =
  scaleBy max

scaleBy :: (Double -> Double -> Double) -- min/max function
        -> V4 Double -- ^ Cell coordinates
        -> V4 Double -- ^ Element coordinates
        -> V4 Double -- ^ Element coordinates after
scaleBy scaleF c@(V4 cX1 cX2 cY1 cY2) e@(V4 eX1 eX2 eY1 eY2) =
  (V4 eX1' eX2' eY1' eY2')
  where
    (cW, cH) = (cX2 - cX1, cY2 - cY1)
    (eW, eH) = (eX2 - eX1, eY2 - eY1)
    (V4 dL dR dT dB) =
      abs (c-e)
    (V2 dW dH) =
      (V2 dL dT) + (V2 dR dB)

    sclX = if eW == 0 then 0 else cW/eW
    sclY = if eH == 0 then 0 else cH/eH
    scl  = scaleF sclX sclY

    eW' = eW * scl
    eH' = eH * scl
    deW' = eW' - eW
    deH' = eH' - eH

    -- Find the growth on each side
    -- from the ratios of growth allowed
    -- on each side.
    dL' = if dW == 0 then 0 else deW' * dL/dW
    dR' = if dW == 0 then 0 else deW' * dR/dW
    dT' = if dH == 0 then 0 else deH' * dT/dH
    dB' = if dH == 0 then 0 else deH' * dB/dH

    eX1' = eX1 - dL'
    eX2' = eX2 + dR'
    eY1' = eY1 - dT'
    eY2' = eY2 + dB'

sizeStretchX :: V4 Double -- ^ Cell coordinates
             -> V4 Double -- ^ Element coordinates
             -> V4 Double -- ^ Element coordinates after
sizeStretchX (V4 cX1 cX2 cY1 xY2) (V4 eX1 eX2 eY1 eY2) =
  V4 cX1 cX2 eY1 eY2


sizeStretchY :: V4 Double -- ^ Cell coordinates
             -> V4 Double -- ^ Element coordinates
             -> V4 Double -- ^ Element coordinates after
sizeStretchY (V4 _ _ cY1 cY2) (V4 eX1 eX2 _ _) =
  V4 eX1 eX2 cY1 cY2
