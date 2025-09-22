trait Length
{
    pub func int length();
}

impl Length for string
{
    pub func int length()
    {
        var string str = this;
        var int length;
        for (length = 0; str[length] != '\0'; length++) {}

        return length;
    }
}

trait Compare
{
    pub func int compare(string other);
}

impl Compare for string
{
    pub func int compare(string other)
    {
        var string str = this;

        for (int i; i <= str->length(); i++)
            if (str[i] != other[i])
                return str[i] - other[i];

        return str->length() - other->length();
    }
}