module Course1_step5_6 where

import Control.Monad.Reader

type User = String
type Password = String
type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks ((map fst) . filter (\x -> snd x == "123456"))