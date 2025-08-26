class Person
{
    var int age = 10;
    pub var bool isMan = false;

    priv func setAge(int a)
    {
        age = a;
    }
    
    pub func int getAge()
    {
        return age;
    }
}

func int main()
{
    var Person pers = new Person();
    
    return 0;
}