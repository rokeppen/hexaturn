Ik heb twee versies voor de parser geschreven: een gewone GameParser en een versie met foutmeldingen ErrorParser.
De gewone versie wordt gebruikt om de commands te verwerken, en heb ik niet verder aangepast naar een versie met Errors.
De ErrorParser is een gelifte versie van de gewone parser en geeft de Error terug die het grootste deel van de file verwerkt heeft. 
Soms geeft dit minder inzichtelijke errors, waardoor er misschien beter een volgorde op de errors kan worden opgelegd, maar ik zie niet direct de optimale volgorde. 
