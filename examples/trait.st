trait Drawable
{
    func draw();
    func float getArea();
}

trait Converter
{
    func string toString();
}

impl Converter for int
{
    pub func string toString()
    {
        return "123";
    }
}

class Circle
{
    priv var float radius;
    
    pub constructor(float r)
    {
        this->radius = r;
    }
    
    pub func float getRadius()
    {
        return this->radius;
    }
}

class Rectangle
{
    priv var float width;
    priv var float height;
    
    pub constructor(float w, float h)
    {
        this->width = w;
        this->height = h;
    }
    
    pub func float getWidth()
    {
        return this->width;
    }
    
    pub func float getHeight()
    {
        return this->height;
    }
}

impl Drawable for Circle
{
    pub func draw()
    {
        echo "Draw circle with radius: ";
        echo this->getRadius();
        echo '\n';
    }
    
    pub func float getArea()
    {
        return 3.14159f * this->getRadius() * this->getRadius();
    }
}

impl Drawable for Rectangle
{
    pub func draw()
    {
        echo "Draw rectangle: ";
        echo this->getWidth();
        echo " x ";
        echo this->getHeight();
        echo '\n';
    }
    
    pub func float getArea()
    {
        return this->getWidth() * this->getHeight();
    }
}

func int main()
{
    var Circle circle = new Circle(5.0f);
    var Rectangle rect = new Rectangle(4.0f, 6.0f);
    
    circle->draw();
    echo "Area of circle: ";
    echo circle->getArea();
    echo '\n';
    
    rect->draw();
    echo "Area of rectangle: ";
    echo rect->getArea();
    echo '\n';
    
    return 0;
}
