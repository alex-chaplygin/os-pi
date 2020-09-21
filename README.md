# os-pi

## Настройка git

Для начала работы:

	git clone https://github.com/alex-chaplygin/jpeg-cs-92_1


Настройка удалённого репозитория

	git remote set-url origin https://github.com/alex-chaplygin/jpeg-cs-92_1




Чтобы проверить правильность параметров предыдущего пункта:

	git remote -v

Там должны быть строчки:

>origin  https://github.com/alex-chaplygin/jpeg-cs-92_1 (fetch)
>
>origin  https://github.com/alex-chaplygin/jpeg-cs-92_1 (push)
	



Настройка профиля:
	
	git config --global user.name "ВАШЕ ИМЯ НА GITHUB"
	git config --global user.email "ВАША ПОЧТА"
	
Настройка ядра git:
	
	git config --system core.autocrlf input
	git config core.repositoryformatversion 0
	git config core.filemode true
	git config core.bare false
	git config core.logallrefupdates true
	

***
## Порядок работы

1. Смотри задание в разделе Issues

2. Синхронизировать удаленное хранилище и локальное

git pull origin master

3. Создаем ветку

git branch iss<номер>

git checkout iss<номер>

4. Работаем

5. Фиксация изменений

git commit -am "Сообщение"

6. Загрузить ветку на удаленный репозиторий

git push origin iss<номер>

7. После завершения тестирования и слияния удалить ветку

git branch -d iss<номер>

git push origin --delete iss<номер>
