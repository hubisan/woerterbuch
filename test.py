def greet_person(person_name):
    return f"Hello, {person_name}!"

def process_command(command, args):
    functions = {
        "greet": greet_person
    }
    
    if command in functions:
        return functions[command](args)
    else:
        return "Invalid command"

while True:
    line = input()
    command, args = line.split(",", 1)
    result = process_command(command, args)
    print(result)
