import maulen

while True:
	text = input('maulen > ')
	if text.strip() == "": continue
	result, error = maulen.run('<stdin>', text)

	if error:
		print(error.como_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))
