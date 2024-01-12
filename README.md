# Folosirea unui mouse cu un fpga
## Descriere Proiect
Am implementat conectarea unui mouse folosind un cablu ps/2(placa basys 3 poate traduce automat semnalul de la usb in comenzi pentru ps/2). Scopul este de a numara clickurile de la mouse, un click stanga creste contorul si un click dreapta il descreste. Afisam pe un ecran seven segment numarul de apasari.

Schematic:  
![Alt text](https://github.com/silviubalan1/fgpa-si-mouse/blob/main/schematic%20fpga%20si%20mouse.PNG?sanitize=true)
Entitati:
* ps2_si_decoder
  * ps2_mouse
  * decoder_mouse  
* nr_si_ssd_complet
  * divizor_de_frecventa
  * nr_bidirectional
  * binary_to_decimal_converter
  * bcd_to_ssd
  * ssd
