### Основное задание
1. Посмотреть содержимое папки;

![image](https://user-images.githubusercontent.com/63774400/145720602-1184b337-b52d-4f4b-9141-9ad0e9b01f24.png)

2. Создать папку;

![image](https://user-images.githubusercontent.com/63774400/145720596-8a80bad8-fcc7-4048-bf80-4e35c9625020.png)

![image](https://user-images.githubusercontent.com/63774400/145720597-447d203b-a118-4192-84ab-b60f4a46fe06.png)

3. Удалить папку;

![image](https://user-images.githubusercontent.com/63774400/145720619-21a7c571-da70-4200-8f46-02441f282c74.png)

4. Удалить файл;

![image](https://user-images.githubusercontent.com/63774400/145720643-8d25eafa-9e5b-4517-8143-2aaab1b2a9d9.png)

5. Переименовать файл;

![image](https://user-images.githubusercontent.com/63774400/145720669-830bb47d-570e-4a34-ae6b-ccd8333ef3c5.png)

6. Скопировать файл с клиента на сервер;

![image](https://user-images.githubusercontent.com/63774400/145732451-7249e5d5-c678-44bc-8b3a-a775ed062648.png)

![image](https://user-images.githubusercontent.com/63774400/145732474-9c5fb68e-436d-47a5-a052-f5f2b2847470.png)

7. Скопировать файл с сервера на клиент;

![image](https://user-images.githubusercontent.com/63774400/145732483-4d6ee38e-404e-4c79-9119-362446e7ff27.png)

![image](https://user-images.githubusercontent.com/63774400/145732511-e30c4272-fbe2-40ee-9319-70cb2189d90f.png)

8. Выход (отключение клиента от сервера);

![image](https://user-images.githubusercontent.com/63774400/145732535-efae7566-c4c7-4ceb-9af7-f17ef6672af2.png)

### Дополнительные задания:

1. Ограничьте возможности пользователя рамками одной определенной директории. Внутри нее он может делать все, что хочет: создавать и удалять любые файлы и папки. Нужно проследить, чтобы пользователь не мог совершить никаких действий вне пределов этой директории. Пользователь, в идеале, вообще не должен догадываться, что за пределами этой директории что-то есть.

Переходим в следующую дерикторию, после переходим назад, и еще раз назад. Дальше свой дериктории пользователь не выходит.

![image](https://user-images.githubusercontent.com/63774400/145732562-90a2c124-90eb-4cee-840a-f056c1cbcd13.png)

2. Добавьте логирование всех действий сервера в файл. Можете использовать разные файлы для разных действий, например: подключения, авторизации, операции с файлами.

![image](https://user-images.githubusercontent.com/63774400/145732893-876234fe-f2ab-4ff1-a7d4-fe097a955096.png)

3. Добавьте возможность авторизации пользователя на сервере.

![image](https://user-images.githubusercontent.com/63774400/145732917-412eac1e-0e6a-4ad1-894b-9d98e4d9c611.png)

4. Добавьте возможность регистрации новых пользователей на сервере. При регистрации для пользователя создается новая рабочая папка (проще всего для ее имени использовать логин пользователя) и сфера деятельности этого пользователя ограничивается этой папкой.

![image](https://user-images.githubusercontent.com/63774400/145732934-ddd7ab7a-5ac0-42cf-a058-7f21cfa3dced.png)

![image](https://user-images.githubusercontent.com/63774400/145733005-77d31da2-985d-4881-9165-20900f84df22.png)

5. Реализуете квотирование дискового пространства для каждого пользователя.

Пользователь не можнт загрузить файлы, если используемое им пространство будет привышать 10МБ

![image](https://user-images.githubusercontent.com/63774400/145733034-1071e292-4d94-4e04-96cc-063c139e58f8.png)

6. Реализуйте учётную запись администратора сервера.

![image](https://user-images.githubusercontent.com/63774400/145733081-14a4476a-929e-4bb8-8b70-4d9e9855d069.png)

![image](https://user-images.githubusercontent.com/63774400/145733098-c7baa0f4-af94-4514-8146-d4e94244501e.png)

7. Напишите отладочный клиент. Клиент должен подключаться к серверу и в автоматическом режиме тестировать корректность его работы. Используйте подход, аналогичный написанию модульных тестов. Клиент должен вывести предупреждающее сообщение, если сервер работает некорректно. 

![image](https://user-images.githubusercontent.com/63774400/145733349-02db057a-5524-4661-ac94-79f3cea43fa6.png)
