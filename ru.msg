# translated by mmmikhailov
# ru_RU (tkw3x-001) UTC 17/1/2018
set ru_data {default;по умолчанию
TkWallet3x for RCoinX (...);TkWallet3x для RCoinX (...)
Anonymous Message;Анонимное сообщение
An anonymous message can be sent instead of coins.\nIt costs only the network transaction fee and the recipient will not know who the message is from.\nThe 'amount' field will be disregarded if you send an anonymous message.\n;Анонимное сообщение можно отправить вместо монет.\nСтоимость его равна сумме комиссии сети и получатель не сможет узнать от кого оно.\n Поле 'количество' будет проигнорировано, если Вы отправите анонимное сообщение.\n
Help;Помощь
Wallet Creation;Создание кошелька
You are creating a new wallet. Enter your desired password below.;Вы создаете новый кошелек. Введите жалемый пароль ниже.
Password:;Пароль:
Confirm:;Подтвердить:
Create Wallet;Создать кошелек
Passwords do not match;Пароли не совпадают
Wallet Login;Логин кошелька
You are accessing a wallet. Enter your password below.;Вы входите в кошелек. Введите Ваш пароль ниже.
Password:;Пароль:
Login;Логин
Working;Работаю
Waiting for RCoinX daemon...;Ожидание демона RCoinX...
Failed to start daemon.\nDid you supply an incorrect password?;Не удалось запустить демон.\nВы ввели неверный пароль?
difficulty;сложность
incoming_connections_count;количество_входящих_соединений
white_peerlist_size;размер_белого_списка_пиров
Balance: %s\nBlockchain Height: %s\n;Баланс: %s\nВеличина блокчейна: %s\n
Ready;Готово
Subject:;Тема:
Address:;Адрес:
Transfer Coins or Message;Переслать монеты или сообщение
To:;Кому:
Amount:;Количество:
Optional Payment ID:;Дополнительный ID платежа:
Anonymous message:;Анонимное сообщение:
(No Subject);(Без Темы)
Write Anonymous Message;Написать анонимное сообщение
Subject;Тема
Done;Готово
Write Message (instead of sending coins);Написать сообщение (вместо пересылки монет)
Current network fee:;Текущая комиссия сети:
Send Coins or Message;Переслать монеты или сообщение
Wait for syncing to complete.;Ожидание завершения синхронизации.
Fee: %s;Комиссия: %s
Not enough coins or the daemon is syncing.;Недостаточно монет или демон синхронизируется.
(No Subject);(Без Темы)
Received anonymous messages:;Получено анонимное сообщение:}
foreach {k v} [split $::ru_data ";\n"] {
	puts "Translation pair: $k -> $v"
	msgcat::mcset ru [string map [list "\\n" "\n"] $k] [string map [list "\\n" "\n"] $v]
}

