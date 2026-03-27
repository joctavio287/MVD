#!/usr/bin/env bash

# set -e: termina si un comando falla
# set -u: termina si una variable no está definida
# set -o pipefail: captura errores en tuberías
set -euo pipefail

# Obtenemos la ruta absoluta de donde vive este script (04_tutorial/)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Nos movemos a la RAÍZ del proyecto (un nivel arriba de 04_tutorial/)
# Esto es vital para que here::here() encuentre el .Rproj o .here en MVD/
cd "$SCRIPT_DIR/.."

echo "Iniciando Pipeline desde: $(pwd)"

# Ejecutamos los scripts usando rutas relativas desde la raíz
# Esto evita que R se confunda de 'home' si el usuario tiene un perfil de terminal ruidoso
Rscript "04_tutorial/scripts/download_data.R"
Rscript "04_tutorial/scripts/wrangling_data.R"
Rscript "04_tutorial/scripts/make_figures.R"

echo "Pipeline finalizado con éxito en $(pwd)"