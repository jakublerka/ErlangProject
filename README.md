## Prosty monitor zasobów komputera.

Aplikacja monitora zasobów komputera działa w trybie klient-serwer. Wymaga włączenia serwera monitoringu oraz podłączenia klientów przy użyciu skompilowanych plików .BEAM. Aplikacja jest uruchamiana z poziomu konsoli Erl, wymagana jest lokalna instalacja pakietu Erl. 
Do projektu dołączono skrypty PowerShell, które po uruchomieniu, rozpoczną konsolę Erl z predefiniowaną nazwą komputera oraz adresem IP w następującej konfiguracji: **<nazwa_komputera>@<adres_IP>**.

22.11.2025 - aplikacja wymaga aby okno PowerShell (lub CMD), było włączone, może być zminimalizowane, aby działało poprawnie.

### monitor_server
Klasa monitor_server składa się z dwóch modułów: 
* monitor_server,<br>
* alert_manager.<br>
Moduł monitor_server, składa się z komponentów:
- start_link(),
- get_metrics(),
- check_alerts(),
- check_criticals().

start_link() odpowiada za włączenie serwera monitoringu.
get_metrics() przyjmuje dane przesłane od klientów i procesuje je.
check_alerts() sprawdza, czy przesłane dane, znajdują się w wyznaczonym zakresie alertu (zużycie CPU, pamięci RAM na poziomie od 85% do 95%). Jeżeli zakres alertu został przekroczony, alert przesyłany jest do modułu **alert_manager**.
check_criticals() sprawdza, czy przesłane dane, znajdują się w wyznaczonym zakresie krytycznego alertu (zużycie CPU, pamięci RAM na poziomie powyżej 95%). Jeżeli zakres krytycznego alertu został przekroczony, alert przesyłany jest do modułu **alert_manager**.


Moduł alert_manager, składa się z komponentów:
- start_link(),
- handle_cast({alert,...}),
- handle_cast({critical,...}).


### monitor_client

Klasa Monitor_client składa się z jednego modułu:
- monitor_client.

W skład modułu monitor_client wchodzą następujące komponenty:
- start_link(Id),
- handle_info(collect, State),
- timestamp(),
- get_cpu(),
- get_ram_percent(),
- collect_metrics().

Funkcja **collect_metrics()** jest najważniejszą funkcją z punktu widzenia klienta. Dzięki niej, wyzwalane jest zbieranie danych odnośnie zużycia CPU i RAMu, funkcje *get_cpu()* oraz *get_ram_percent()*.



## Rozpoczęcie pracy serwera oraz klienta

### Serwer

Po uruchomieniu konsoli erl, jeżeli znajdujemy się w lokalizacji z plikami .BEAM, należy wpisać następujące komendy, kolejność jest ważna:
1) alert_manager:start_link().
2) monitor_server:start_link().

Konsola erl powinna zwrócić następujące wartości:
Ad 1). 
alert_manager started
{ok, <numer_procesu>}
Ad 2). 
monitor_server started
{ok, <numer_procesu>}


### Klient
Po uruchomieniu konsoli erl, jeżeli znajdujemy się w lokalizacji z plikami .BEAM, należy wpisać następującą komendę:
1) monitor_client:start_link('nazwa_komputera').

Konsola erl powinna zwrócić następującą wartość:
Ad 1) 
monitor_client("nawza_komputera") started
true

Reszta operacji po stronie klienta dzieje się w czasie prawie-rzeczywistym, bez ingerencji w działania użytkownika.

### Troubleshooting

W celach rozwiązywania problemów połączeniowych pomiędzy klientami a serwerem, można skorzystać z następujących komend:

- net_adm:ping('nazwa_komputera'). -> wykonana z poziomu klienta lub serwera, dana zwrotna *pong* oznacza że odpytywany system odpowiada poprawnia. Zwrot *pang* oznacza że komunikacja nie zachodzi pomiędzy hostami.
- nodes(). -> wykonane z poziomu serwera, wyświetla aktualnie podłączonych klientów.