class Person
{
    priv var string name = "Alex";
    priv var int age = 16;
    priv var bool isMale = true;

    pub constructor(string name)
    {
        this->name = name;
    }

    pub constructor(int age)
    {
        this->age = age;
    }

    pub constructor(string name, int age)
    {
        this->name = name;
        this->age = age;
    }

    priv func setAge(int age)
    {
        this->age = age;
    }
    
    pub func int getAge()
    {
        return age;
    }

    pub func void print()
    {
        if (isMale) echo "Male";
        else echo "Female";

        echo name;
        echo age;
    }
}

func int main()
{
    var Person nikita = new Person("Nikita");
    var Person alex = new Person(25);
    var Person tim = new Person("Tim", 18);

    nikita->print();
    
    return 0;
}