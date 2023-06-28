module YIBP.ToDomain (ToDomain (..)) where


class ToDomain from to where
  toDomain :: from -> to
