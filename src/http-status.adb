
package body HTTP.Status is

 function AWS_StatusCode_From_String
       (StatusCode_String : in String) return AWS.Messages.Status_Code
    is
        use AWS.Messages;

        Status : AWS.Messages.Status_Code;
    begin

        if StatusCode_String = "100" then
            Status := S100;
        elsif StatusCode_String = "101" then
            Status := S101;
        elsif StatusCode_String = "102" then
            Status := S102;
        elsif StatusCode_String = "200" then
            Status := S200;
        elsif StatusCode_String = "201" then
            Status := S201;
        elsif StatusCode_String = "202" then
            Status := S202;
        elsif StatusCode_String = "203" then
            Status := S203;
        elsif StatusCode_String = "204" then
            Status := S204;
        elsif StatusCode_String = "205" then
            Status := S205;
        elsif StatusCode_String = "206" then
            Status := S206;
        elsif StatusCode_String = "207" then
            Status := S207;
        elsif StatusCode_String = "208" then
            Status := S208;
        elsif StatusCode_String = "226" then
            Status := S226;
        elsif StatusCode_String = "300" then
            Status := S300;
        elsif StatusCode_String = "301" then
            Status := S301;
        elsif StatusCode_String = "302" then
            Status := S302;
        elsif StatusCode_String = "303" then
            Status := S303;
        elsif StatusCode_String = "304" then
            Status := S304;
        elsif StatusCode_String = "305" then
            Status := S305;
        elsif StatusCode_String = "306" then
            Status := S306;
        elsif StatusCode_String = "307" then
            Status := S307;
        elsif StatusCode_String = "308" then
            Status := S308;
        elsif StatusCode_String = "400" then
            Status := S400;
        elsif StatusCode_String = "401" then
            Status := S401;
        elsif StatusCode_String = "402" then
            Status := S402;
        elsif StatusCode_String = "403" then
            Status := S403;
        elsif StatusCode_String = "404" then
            Status := S404;
        elsif StatusCode_String = "405" then
            Status := S405;
        elsif StatusCode_String = "406" then
            Status := S406;
        elsif StatusCode_String = "407" then
            Status := S407;
        elsif StatusCode_String = "408" then
            Status := S408;
        elsif StatusCode_String = "409" then
            Status := S409;
        elsif StatusCode_String = "410" then
            Status := S410;
        elsif StatusCode_String = "411" then
            Status := S411;
        elsif StatusCode_String = "412" then
            Status := S412;
        elsif StatusCode_String = "413" then
            Status := S413;
        elsif StatusCode_String = "414" then
            Status := S414;
        elsif StatusCode_String = "415" then
            Status := S415;
        elsif StatusCode_String = "416" then
            Status := S416;
        elsif StatusCode_String = "417" then
            Status := S417;
        elsif StatusCode_String = "418" then
            Status := S418;
        elsif StatusCode_String = "421" then
            Status := S421;
        elsif StatusCode_String = "422" then
            Status := S422;
        elsif StatusCode_String = "423" then
            Status := S423;
        elsif StatusCode_String = "424" then
            Status := S424;
        elsif StatusCode_String = "425" then
            Status := S425;
        elsif StatusCode_String = "426" then
            Status := S426;
        elsif StatusCode_String = "428" then
            Status := S428;
        elsif StatusCode_String = "429" then
            Status := S429;
        elsif StatusCode_String = "431" then
            Status := S431;
        elsif StatusCode_String = "451" then
            Status := S451;
        elsif StatusCode_String = "500" then
            Status := S500;
        elsif StatusCode_String = "501" then
            Status := S501;
        elsif StatusCode_String = "502" then
            Status := S502;
        elsif StatusCode_String = "503" then
            Status := S503;
        elsif StatusCode_String = "504" then
            Status := S504;
        elsif StatusCode_String = "505" then
            Status := S505;
        elsif StatusCode_String = "506" then
            Status := S506;
        elsif StatusCode_String = "507" then
            Status := S507;
        elsif StatusCode_String = "508" then
            Status := S508;
        elsif StatusCode_String = "510" then
            Status := S510;
        elsif StatusCode_String = "511" then
            Status := S511;
        end if;

        return Status;

    end AWS_StatusCode_From_String;

end HTTP.Status;