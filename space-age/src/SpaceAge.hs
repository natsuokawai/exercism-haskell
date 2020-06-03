module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

planetYear :: Planet -> Float
planetYear Mercury = 0.2408467
planetYear Venus   = 0.61519726
planetYear Earth   = 1.0
planetYear Mars    = 1.8808158
planetYear Jupiter = 11.862615
planetYear Saturn  = 29.447498
planetYear Uranus  = 84.016846
planetYear Neptune = 164.79132

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (31557600 * planetYear planet)
