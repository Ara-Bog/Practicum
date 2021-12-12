import socket
import re
import os

HOST = 'localhost'
PORT = 8080
END_FLAG = b"$$STREAM_FILE_END_FLAG$$"
FAIL_FLAG = b'$FAILED$'
login = "tester"
password = "123456"
# login = "admin"
# password = "admin"
current_directory = "\\"

def creator(message, size=0):
    global login, password, current_directory
    return f"{login}=login{password}=password{current_directory}=cur_dir{size}=file_size{message}".encode()


def sending(request):
    global sock, FAIL_FLAG, END_FLAG
    
    flag_finder = sock.recv(1024)
    if FAIL_FLAG in flag_finder:
        print((flag_finder.replace(FAIL_FLAG, b"")).decode())
    else:
        filename = re.split("[ \\/]+", request)[-1]
        with open (filename, "wb") as bytefile:
            while True:
                if END_FLAG in flag_finder:
                    flag_finder, end_flag_msg = flag_finder.split(END_FLAG)
                    bytefile.write(flag_finder.replace(END_FLAG, b""))
                    break
                else:
                    bytefile.write(flag_finder)
                    flag_finder = sock.recv(1024)
    print(sock.recv(1024).decode())
    

def receiving(request):
    global sock, END_FLAG
    filename = re.split("[ \\/]+", request)[-1]
    if os.path.exists(filename):
        size = os.path.getsize(filename)
        sock.send(creator(request, size))
        enought_flag = sock.recv(1024).decode()
        if enought_flag != '$ENOUGHT$':
            print(enought_flag)
            return False
        with open(filename, "rb") as bytefile:
    
            while read_bytes := bytefile.read(1024):
                sock.send(read_bytes)
        sock.send(END_FLAG)
        print(sock.recv(1024).decode())
        return True
    else:
        print("Нет такого файла")
        sock.send(creator("error"))
        return False


test_list = [
    "mkdir test1",
    "cd ...../test1",
    "cd ./test1",
    "mkdir ../test1/test11",
    "ls",
    "pwd",
    "rmtree test11",
    "ls",
    "touch 1.txt",
    "rename 1.txt 11.txt",
    "ls",
    "remove 1.txt",
    "cat 1.txt",
    "cat 11.txt",
    "cd ////",
    "pwd",
    "cd \\",
    "pwd",
    "rmtree test1",
    "ls"
]

for command in test_list:
    try:
        print(">", command)
        request = command
        if request == "exit":
            print("goodbye")
            break
        arrRequest = request.split(" ")
        sock = socket.socket()
        sock.connect((HOST, PORT))
        if arrRequest[0] == "get_file":
            if not receiving(request):
                continue
        else:
            sock.send(creator(request))
            if arrRequest[0] == "send_file":
                sending(request)
            else:
                response = sock.recv(1024).decode()
                if arrRequest[0] == "cd":
                    current_directory = response
                else:
                    print(response)
        sock.close()
    except:
        print('Некорректная работа!')
        raise
input("Нажмите Enter чтобы закрыть")