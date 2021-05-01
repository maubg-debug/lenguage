import learner
import sys

def main():
  argv = sys.argv

  if len(argv) == 2:

    # file = argv[1].replace('.\\', '').replace('./', '')
    
    file = argv[1]
    try:
      result, error = learner.run(f'<{file}>', f'run("{file}")')
    except KeyboardInterrupt:
      exit(0)

    if error:
      print(error.como_string())
  else:
    while True:
      try:
        texto = input('>>> ')
        if texto.strip() == "":
          continue
        result, error = learner.run('<stdin>', texto)

        if error:
          print(error.como_string())
        elif result:
            if len(result.elements) == 1:
              print(repr(result.elements[0]))
            else:
              print(repr(result))
      except KeyboardInterrupt:
        print("\n\n - pon exit() para salir - \n")

if __name__ == "__main__":
  main()