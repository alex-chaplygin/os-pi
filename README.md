# os-pi

## Настройка git

Для начала работы:

	git clone https://github.com/alex-chaplygin/os-po


Настройка удалённого репозитория

	git remote set-url origin https://github.com/alex-chaplygin/os-pi







Настройка профиля:
	
	git config --global user.name "ВАШЕ ИМЯ НА GITHUB"
	git config --global user.email "ВАША ПОЧТА"

	

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
