from lib import lib

# You should avoid sharing this token,
#  and should store it in an env variable
sms = lib.utils.sms["@1.0.9"]

result = sms(
  to="7789843955", # (required)
  body="PCB Soldering completed!" # (required)
)
