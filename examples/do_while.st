func int main()
{
    var int i = 0;

    do
    {
        if (i == 5)
        {
            i += 1;
            continue;
        }
        
        echo i;
        i += 1;
    } -> (i < 10);
    
    return 0;
}