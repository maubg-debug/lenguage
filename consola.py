import maulen

while True:
	texto = input('maulen > ')
	if texto.strip() == "": continue
	result, error = maulen.run('<stdin>', texto)

	if error:
		print(error.como_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))
