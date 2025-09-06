class Point
{
    var int x;
    var int y;
    
    constructor(int x, int y)
    {
        this->x = x;
        this->y = y;
    }
    
    func int getX()
    {
        return this->x;
    }
    
    func int getY()
    {
        return this->y;
    }
}

func int main()
{
    var Point[] points = [new Point(1, 2), new Point(3, 4), new Point(5, 6)];
    
    for (int i; i < 3; i += 1)
    {
        echo '(';
        echo points[i]->x;
        echo "; ";
        echo points[i]->getY();
        echo ')';
        if (i != 2) echo ", ";
    }
    echo '\n';
    
    return 0;
}
