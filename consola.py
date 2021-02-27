import maulen
import sys

argv = sys.argv

if len(argv) == 2:
    file = argv[1].replace('.\\', '').replace('./', '')
    result, error = maulen.run(f'<{file}>', f'run("{file}")')

    if error:
        print(error.como_string())
    elif result:
        if len(result.elements) == 1:
            print("\n - Se ha finalizado el programa :: " +
                  repr(result.elements[0]) + " - \n")
        else:
            print("\n Se ha finalizado el programa :: " + repr(result) + " - \n")
else:
    while True:
        try:
            texto = input('>>> ')
            if texto.strip() == "":
                continue
            result, error = maulen.run('<stdin>', texto)

            if error:
                print(error.como_string())
            elif result:
                if len(result.elements) == 1:
                    print("\n\n - Se ha finalizado el programa :: " +
                          repr(result.elements[0]) + " - \n\n")
                else:
                    print("\n\n Se ha finalizado el programa :: " +
                          repr(result) + " - \n\n")
        except KeyboardInterrupt:
            print("\n - pon exit() para salir - \n")
