import tornado.ioloop
import tornado.web
import os

static_path = os.path.join(os.path.dirname(__file__), "output")
settings = {
    "static_path" : static_path
}

class MainHandler(tornado.web.RequestHandler):
    def get(self):
        self.write("Hello, world")

def make_app():
    return tornado.web.Application([
        (r'/(.*)', tornado.web.StaticFileHandler, {'path': static_path}),
    ], **settings)

if __name__ == "__main__":
    app = make_app()
    app.listen(8090)
    print(static_path)
    tornado.ioloop.IOLoop.current().start()