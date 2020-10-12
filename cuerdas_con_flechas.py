def cuerdas_con_flechas(text, pos_comienzo, pos_final):
	resultado = ''

	# calcular los indices
	idx_start = max(text.rfind('\n', 0, pos_comienzo.idx), 0)
	idx_end = text.find('\n', idx_start + 1)
	if idx_end < 0: idx_end = len(text)
	
	# generar cada linea
	line_count = pos_final.ln - pos_comienzo.ln + 1
	for i in range(line_count):
		# Calculate line columns
		line = text[idx_start:idx_end]
		col_start = pos_comienzo.col if i == 0 else 0
		col_end = pos_final.col if i == line_count - 1 else len(line) - 1

		# añadir el resultado
		resultado += line + '\n'
		resultado += ' ' * col_start + '^' * (col_end - col_start)

		# recalcular los indices
		idx_start = idx_end
		idx_end = text.find('\n', idx_start + 1)
		if idx_end < 0: idx_end = len(text)

	return resultado.replace('\t', '')
