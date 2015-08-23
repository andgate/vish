module Vish.Layout where

import Control.Lens
import Linear.V2 (V2 (..))
import Linear.V4 (V4 (..))
import qualified Linear.V4 as Vec4
import qualified Linear.Vector as Vec

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

data Sizing = None | Fit | Fill | FillX | FillY | CropFill

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
      e = toSides Vec.zero eS
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
  let eW  = (e ^. Vec4._y) - (e ^. Vec4._x)
      eX1' = c^.Vec4._x
      eX2' = eX1' + eW
  in e & (Vec4._x .~ eX1')
       . (Vec4._y .~ eX2')


alignCenterH :: V4 Double -- ^ Cell coords
             -> V4 Double -- ^ Element coords
             -> V4 Double -- ^ Aligned eLement coords
alignCenterH c e =
  let cW = (c ^. Vec4._y) - (c ^. Vec4._x)
      eW = (e ^. Vec4._y) - (e ^. Vec4._x)
      m  = (cW - eW) / 2
      eX1' = (c^.Vec4._x) + m
      eX2' = eX1' + eW
  in e & (Vec4._x .~ eX1')
       . (Vec4._y .~ eX2')


alignRight :: V4 Double -- ^ Cell x1, x2
           -> V4 Double -- ^ Element x1, x2
           -> V4 Double -- ^ ELement x1', x2'
alignRight c e =
  let eW   = (e ^. Vec4._y) - (e ^. Vec4._x)
      eX1' = eX2' - eW
      eX2' = c^.Vec4._y
  in e & (Vec4._x .~ eX1')
       . (Vec4._y .~ eX2')


alignTop :: V4 Double -- ^ Cell coords
         -> V4 Double -- ^ Element coords
         -> V4 Double -- ^ Aligned eLement coords
alignTop c e =
 let eH  = (e ^. Vec4._w) - (e ^. Vec4._z)
     eY1' = c^.Vec4._z
     eY2' = eY1' + eH
 in e & (Vec4._z .~ eY1')
      . (Vec4._w .~ eY2')


alignCenterV :: V4 Double -- ^ Cell coords
             -> V4 Double -- ^ Element coords
             -> V4 Double -- ^ Aligned eLement coords
alignCenterV c e =
 let cH = (c^.Vec4._w) - (c^.Vec4._z)
     eH = (e^.Vec4._w) - (e^.Vec4._z)
     m  = (cH - eH) / 2
     eY1' = (c^.Vec4._z) + m
     eY2' = eY1' + eH
 in e & (Vec4._z .~ eY1')
      . (Vec4._w .~ eY2')


alignBottom :: V4 Double -- ^ Cell x1, x2
            -> V4 Double -- ^ Element x1, x2
            -> V4 Double -- ^ ELement x1', x2'
alignBottom c e =
 let eH   = (e ^. Vec4._w) - (e ^. Vec4._z)
     eY1' = eY2' - eH
     eY2' = c^.Vec4._w
 in e & (Vec4._z .~ eY1')
      . (Vec4._w .~ eY2')


size :: Sizing   -- ^ Sizing to adjust element to
     -> V4 Double -- ^ Cell coordinates
     -> V4 Double -- ^ Element coordinates
     -> V4 Double -- ^ Element coordinates after
size None     _ = id
size Fit      c = sizeFit c
size Fill     c = sizeFillX c . sizeFillY c
size FillX    c = sizeFillX c
size FillY    c = sizeFillY c
size CropFill c = sizeCropFill c


sizeFit :: V4 Double -- ^ Cell coordinates
        -> V4 Double -- ^ Element coordinates
        -> V4 Double -- ^ Element coordinates after
sizeFit c@(V4 cX1 cX2 cY1 cY2) e@(V4 eX1 eX2 eY1 eY2)
  | dX < dY =
      V4 (eX1+dX1) (eX2+dX2) (eY1+dX/2) (eY2+dX/2)
  | otherwise =
      V4 (eX1+dY/2) (eX2+dY/2) (eY1+dY1) (eY2+dY2)
  where
    (V4 dX1 dX2 dY1 dY2) =
      V4 (cX1-eX1) (eX2-cX2) (cY1-eY1) (eY2-cY2)
    (V2 dX dY) =
      (abs $ V2 dX1 dX2) + (abs $ V2 dY1 dY2)


sizeFillX :: V4 Double -- ^ Cell coordinates
          -> V4 Double -- ^ Element coordinates
          -> V4 Double -- ^ Element coordinates after
sizeFillX (V4 cX1 cX2 cY1 xY2) (V4 eX1 eX2 eY1 eY2) =
  V4 cX1 cX2 eY1 eY2


sizeFillY :: V4 Double -- ^ Cell coordinates
          -> V4 Double -- ^ Element coordinates
          -> V4 Double -- ^ Element coordinates after
sizeFillY (V4 _ _ cY1 cY2) (V4 eX1 eX2 _ _) =
  V4 eX1 eX2 cY1 cY2

sizeCropFill :: V4 Double -- ^ Cell coordinates
             -> V4 Double -- ^ Element coordinates
             -> V4 Double -- ^ Element coordinates after
sizeCropFill (V4 cX1 cX2 cY1 xY2) e@(V4 eX1 eX2 eY1 eY2) =
  e
