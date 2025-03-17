#!/bin/bash
# Run eldantic examples

echo "Running basic usage example:"
emacs --batch -l eldantic.el -l examples/basic-usage.el

echo -e "\nRunning API response example:"
emacs --batch -l eldantic.el -l examples/api-response.el