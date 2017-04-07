import tornado.ioloop
import tornado.web as web
import tornado.gen as gen
import os

static_path = os.path.join(os.path.dirname(__file__), "output")
settings = {
    "static_path" : static_path
}


class MainHandler(tornado.web.RequestHandler):

    @gen.coroutine
    def get(self):
        self.redirect("/demo.htm")

def make_app():
    return tornado.web.Application([
        (r'/', MainHandler),
        (r'/(.*)', web.StaticFileHandler, {'path': static_path}),
    ], **settings)

if __name__ == "__main__":
    app = make_app()
    app.listen(8888)
    print(static_path)
    tornado.ioloop.IOLoop.current().start()