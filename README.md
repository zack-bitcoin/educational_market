Educational Market
=====

The purpose of this project is to help people learn about prediction markets.

turn it on:
```sh start.sh```

the page for users can be seen here:
http://127.0.0.1:8000/home.html

the page for admins is here:
http://127.0.0.1:8000/admin.html

it automatically starts in the background. You can attach to it to be able to run commands like this:
```sh attach.sh```

After attaching, you can turn the system off and preserve the current state like this:
```utils:off().
halt().
```

or you can detatch from the running process, and return it to the background by holding the Control key, and clicking the 'D' key.

To add your pubkey to the list of admin accounts:
```
admin:add(base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4=")).
```